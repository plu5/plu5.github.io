---
layout: post
title: 11 — Coercing OneDrive API
date: 2026-03-27 16:20
modified_date: 2026-04-07 20:48
categories: microsoft api onedrive onedriveverrer
lang: en
redirect_from: /devlog/11
---

(Note to self: cf 2026-02-22)

## I can't get access
I've had the idea to make a script to download a previous version of a _folder_ (back to a particular date) on OneDrive (with the site you can download a previous version of a file, but not a folder), but I'm having a very hard time trying to access the API. If I understand correctly, it's not possible anymore without paying a subscription? There is an Azure "free" account, which requires extensive personal details and a credit card, but the FAQs suggest you get locked out after 30 days.

- account.live.com/developers/applications : no longer available
- aka.ms/AppRegistrations : not available to personal accounts, need a developer account or Azure subscription

It's possible to log on to entra.microsoft.com using a personal account, but you get stuck in an error loop, which is [apparently by design](https://learn.microsoft.com/en-us/answers/questions/2224214/persistent-error-aadsts160021-application-requeste). I bypassed the error dialogs client-side, but it doesn't let you create an application or group, most of the UI does nothing or leads to an error page.

> L'extension a rencontré une erreur inattendue et n'a fourni aucun détail supplémentaire.

[There used to be a developer program](https://www.reddit.com/r/AZURE/comments/15vbqhc), but [it seems not anymore](https://www.reddit.com/r/Office365/comments/1afn2vf/), only for entreprises or Visual Studio Professional/Entreprise subscribers.

Hours into this I am not finding any path that hasn't been blocked.

> C'est quoi ce cauchemar kafkaïen ?

## What is needed in practice to authenticate
I looked at OneDrive clients by [skilion] and [abraunegg], [rclone], and [this merge.dev article][merge.dev].

In practice, in a request you need:
- The access token in "Authorization" header
- In theory, a conforming User-Agent to avoid getting blocked or throttled ([docs](https://learn.microsoft.com/en-us/sharepoint/dev/general-development/how-to-avoid-getting-throttled-or-blocked-in-sharepoint-online#how-to-decorate-your-http-traffic-to-avoid-throttling), [SO](https://stackoverflow.com/questions/49654651/what-should-user-agent-be-set-to-when-using-microsoft-graph-api))

Access tokens expire in 3599 seconds after we receive them (1 hour minus a second) according to the `expires_in` information we receive when we generate one. Yet I once saw in the logs the following error, which made me think they last 24 hours:
```sh
ERROR: Microsoft OneDrive API returned an error with the following message:
  Error Message:       HTTP request returned status code 401 (Unauthorized)
  Error Reason:        The access token has expired. It's valid from '3/18/2026 4:07:35 PM' and to '3/19/2026 4:07:35 PM'.
  Error Code:          unauthenticated
```

Refresh tokens live longer, and the skilion and abraunegg OneDrive clients save them to disk, unlike the access token which they only save to memory. But for a single refresh token, can there only be a single valid access token, or can there be several? I mean if you use a OneDrive client and have it running and then separately use the refresh token to generate an access token yourself, will that interfere with the client, cause the access token it's currently using to get revoked?

{% include note.html content='
> [!NOTE]
> No. It seems there can be several tokens valid at the same time, as I discover in [the subsection after next](#using-a-refresh-token).
' %}

## Using just an existing access token
abraunegg's OneDrive client has argument `--print-access-token`, but it doesn't work by itself. It's just one of several conditions to pass that you can see in the function `debugOutputAccessToken` in `onedrive.d`
1. `appConfig.verbosityCount > 1`
2. `appConfig.getValueBool("debug_https")`
3. `appConfig.getValueBool("print_token")`
4. `debugLogging`

2 is controlled by CLI option `--debug-https`, 3 is controlled by CLI option `--print-access-token`. As for 1 and 4, they are related; if `verbosityCount >= 2`, `debugLogging` is set to true (see [`main.d` L160](https://github.com/abraunegg/onedrive/blob/aa3389c660c67001573a1e285f57a9a068608b97/src/main.d#L160)). `verbosityCount` is controlled by CLI option `--verbose`, with the count representing the number of times it appears. So for the access token to be printed, the options needed are: `--verbose --verbose --debug-https --print-access-token`, and then also `--monitor` (`-m`) or `--sync`, as without one of those two options it will just quit without doing anything.

Something like:
```sh
onedrive --verbose --verbose --debug-https --print-access-token -m > ~/OneDrive-log.txt 2>&1 &disown
```

With this we also see all the request headers and endpoints, which will allow us to easily replicate them.

Look for the token in the log by searching for "CAUTION - KEEP THIS SAFE: Current access token:".

Simple test request, adapted from [merge.dev] `validate_token` (replace with your token and user agent):
```python
#!/usr/bin/env python3
import requests

TOKEN = "[..token..]"
USERAGENT = "[..user agent..]"

def get_me():
    headers = {"authorization": f"bearer {TOKEN}", "user-agent": USERAGENT}
    r = requests.get("https://graph.microsoft.com/v1.0/me/drive", headers=headers)
    if r.status_code == 200:
        return r.json()
    else:
        print(r.json())
        r.raise_for_status()

if __name__ == "__main__":
    print(get_me())
```
Notes:
- The capitalisation on the header names and the "bearer" [doesn't matter](https://stackoverflow.com/questions/5258977/are-http-headers-case-sensitive)
- In [merge.dev] he uses endpoint `/v1.0/me` rather than `/v1.0/me/drive`. For me the former always fails with 401 regardless of validity of token.
- In [merge.dev] he does not set a user agent.
- I used `raise_for_status` instead of something like `raise RuntimeError(f"Request failed: {r.status_code}")` because we get more information about the error with the former, and `print(r.json())` because with Microsoft API requests the response contains more useful information about the error. I wish I could pass that information to the exception in `raise_for_status`, but alas you can't. This will fail if you get a 404 or something that will have no response json. I guess it would be better to wrap it in a try catch and print just the status code or something if there is no json (`requests.exceptions.JSONDecodeError`) (although `raise_for_status` will show the status code in its exception anyway, it's sufficient to just avoid the json exception before it can do raise its own exception), but I will leave it as is for simplicity.

The log will quickly become huge (the file grows at a rate of about 60M per hour for me, idling), so you do not want to keep running onedrive with the verbose settings for too long.

## Using a refresh token
Things needed to generate an access token from a refresh token:
1. client ID (also known as app ID)
2. redirect URI
3. refresh token

1 and 2 are exposed publicly in open source OneDrive clients

A refresh token is stored in `~/.config/onedrive/refresh_token` in abraunegg's and skilion's OneDrive clients.

```python
#!/usr/bin/env python3
import requests
from os.path import expanduser

CLIENTID = "[..client id..]"
USERAGENT = "[..user agent..]"
REFRESHTOKENFILE = expanduser("~/.config/onedrive/refresh_token")
refreshtoken = ""

with open(REFRESHTOKENFILE, "r") as f:
    refreshtoken = f.read()
    if not refreshtoken:
        raise RuntimeError("No refresh token")

def get_token():
    d = {"client_id": CLIENTID,
         "redirect_uri": "https://login.microsoftonline.com/common/oauth2/nativeclient",
         "refresh_token": refreshtoken,
         "grant_type": "refresh_token"}
    r = requests.post("https://login.microsoftonline.com/common/oauth2/v2.0/token",
                      headers={"user-agent": USERAGENT}, data=d)
    if r.status_code == 200:
        return r.json()
    else:
        print(r.json())
        r.raise_for_status()

if __name__ == "__main__":
    print(get_token())
```
Notes:
- The expanduser import is just to expand the tilde, you can remove it and just have a simple string there if you specify an absolute path.
- The "common" in the request endpoint and redirect URI is the tenant ID (which [can be "common"](https://stackoverflow.com/questions/43288610/endpoints-common-vs-tenant-id))
  + I don't really understand what tenants mean. There is also the concept of single- vs multi-tenant applications.

We get:
```python
{'token_type': 'Bearer', 'scope': 'Files.ReadWrite Files.ReadWrite.All', 'expires_in': 3599, 'ext_expires_in': 3599, 'access_token': '[..access token..]', 'refresh_token': '[..refresh token..]'}
```

We receive a refresh token which can be different than the one we used to make the request. abraunegg's client uses this to update the refresh token on disk each time.

If I have the OneDrive client (whose client ID I used) running in the background, I get a different access token to the one it has, and it continues to use the one it has without issues. So it seems there can be several access tokens valid at the same time without interference.

## Generating our own refresh token (standard auth flow)
Things needed to generate a refresh token:
1. client ID
2. redirect URI
3. scope

[merge.dev] also has app secret. I'm not sure when/why this is needed. Maybe it's optional if you have your own app and want to avoid other people making requests with your client ID.

[rclone] has one too, and they encrypt it with AES-CTR (just to obfuscate I guess)

Following the same steps as [abraunegg]'s OneDrive client:
```python
#!/usr/bin/env python3
import re
import requests

CLIENTID = "[..client id..]"
USERAGENT = "[..user agent..]"
SCOPE = "Files.ReadWrite Files.ReadWrite.All Sites.ReadWrite.All offline_access"
DOMAIN = "https://login.microsoftonline.com/common/oauth2"

def auth():
    print("Please authorise this application by visiting the following URL:")
    d = {"client_id": CLIENTID,
         "redirect_uri": f"{DOMAIN}/nativeclient",
         "scope": SCOPE.replace(" ", "%20"),
         "response_type": "code",
         "prompt": "login"}
    print(f"{DOMAIN}/v2.0/authorize?" +
          "&".join("%s=%s" % (k, v) for k, v in d.items()))
    print("After completing the authorisation in your browser, copy the full "
          "redirect URI (from the address bar) and paste it below.")
    response = input("Paste redirect URI here: ")
    code = re.search(r"(?:[?&]code=)([^&]+)", response.strip())
    if not code:
        raise RuntimeError("An empty or invalid redirect URI was entered")
    d = {"client_id": CLIENTID,
         "redirect_uri": f"{DOMAIN}/nativeclient",
         "code": code.group(1),
         "grant_type": "authorization_code"}
    r = requests.post(f"{DOMAIN}/v2.0/token",
                      headers={"user-agent": USERAGENT},
                      data="&".join("%s=%s" % (k, v) for k, v in d.items()))
    if r.status_code == 200:
        return r.json()
    else:
        print(r.json())
        r.raise_for_status()

if __name__ == "__main__":
    print(auth())
```
Notes:
- The flow here is the same as in abraunegg's client "OAuth2 Interactive Authorisation Flow (application default)". We construct a URL that the user has to open in a browser, then ask them to paste in the URL Microsoft redirects to, which contains the code as a parameter.
- ^ This used to work well, but recently Microsoft has started to redirect to "wrongplace", so you either have to copy the URL quickly before you get redirected, or get the URL that has the code from your browser history. See [abraunegg/onedrive#3558](https://github.com/abraunegg/onedrive/discussions/3558).
- I'm joining the payload into a string manually to avoid `requests` encoding it, which it does if passing the dictionary. This is a particular problem with the code, because the code we receive is already encoded and `requests` re-encodes it, for example turning %24 into %2524, making the code invalid. Another way to handle it would be to decode it after we receive it (only for it to be re-encoded again, but at least not double-encoded) (this is what abraunegg seems to do).
- The comprehension I used to join the payload into a string is by [furas](https://stackoverflow.com/a/23497912).
- For the URL the user has to open in the browser, I initially used `f"{DOMAIN}/v2.0/authorize?" + urllib.parse.urlencode(d, quote_via=urllib.parse.quote)` (the `quote_via` is to get it to encode spaces with %20 instead of + [[1](https://stackoverflow.com/questions/21823965/use-20-instead-of-for-space-in-python-query-parameters), [2](https://github.com/encode/httpx/discussions/2460)]), but we don't actually need to encode anything other than spaces as %20.

The response is the same as when we generate an access token from a refresh token:
```python
{'token_type': 'Bearer', 'scope': 'Files.ReadWrite Files.ReadWrite.All', 'expires_in': 3599, 'ext_expires_in': 3599, 'access_token': '[..access token..]', 'refresh_token': '[..refresh token..]'}
```
The expiry is for the access token, not for the refresh token.

The refresh token I receive is different to the one the client is using, yet the client continues to use its refresh token without issue, so just like access tokens it seems we can have several refresh tokens without interference.

The process followed in [merge.dev] is different, he sets up an HTTP server instead that he has it set up to redirect to and then he can read the code directly instead of the user having to paste the redirect URI. He set the redirect URI (to http://localhost:8080/callback in his case) in Entra, so I can't demonstrate this. Or can I? Can't we just change the redirect URI we give in the URL parameters?

Also it might resolve the "wrongplace" issue to give it a localhost redirect URI instead of the nativeclient page that Microsoft doesn't seem to want to support anymore.

No, it doesn't work:

> Nous ne pouvons pas traiter votre demande  
`invalid_request`: The provided value for the input parameter `'redirect_uri'` is not valid. The expected value is a URI which matches a redirect URI registered for this client application.

I get this on a login.live.com page I get redirected to after trying to log in if the URL has something other than the nativeclient URL in the `redirect_uri` parameter.

## Doing stuff with it
[Graph Explorer](https://developer.microsoft.com/en-us/graph/graph-explorer) is useful (returns sample data, you don't need to be connected, and you can modify the endpoints. that way you can verify you are contacting the right endpoint with the right parameters.)

The basic endpoint, like we had before to test the token:
```python
GRAPHDOMAIN = "https://graph.microsoft.com/v1.0/me/drive"

def headers(token=None):
    """Return standard headers to send with requests."""
    if token:
        return {"authorization": f"bearer {token}", "user-agent": USERAGENT}
    return {"user-agent": USERAGENT}

def get_res(r, json=True):
    """Get json response of a request or print details and raise exception
    if it failed."""
    if r.status_code == 200:
        return r.json() if json else r.content
    else:
        try:
            print(r.json())
        except requests.exceptions.JSONDecodeError:
            print(f"No json body. Status code: {r.status_code}")
        r.raise_for_status()

def get_me(token):
    """Drive details."""
    r = requests.get(f"{GRAPHDOMAIN}", headers=headers(token))
    return get_res(r)
```
Since the other endpoints are subpaths of that, I'm going to add an argument as follows:
```python
def get_me(token, sub=""):
    """Drive details."""
    r = requests.get(f"{GRAPHDOMAIN}{sub}", headers=headers(token))
    return get_res(r)
```

## Listing children and versions
Then we can easily build on it to add other endpoints:
```python
def get_children(token, subpath=""):
    """Drive top-level files, or files in SUBPATH.
    SUBPATH must not begin or end with /."""
    p = f"/root:/{subpath}:/children" if subpath else "/root/children"
    return get_me(token, p)
```
If there are a lot of children, the output could be paginated. Handling of pagination and nice output, adapted from [merge.dev]:
```python
def get_next(token, data):
    """Resolve next link in data. Used for pagination."""
    r = requests.get(data['@odata.nextLink'], headers=headers(token))
    return get_res(r)

def get_all_children(token, subpath=""):
    """get_children wrapper with pagination handling."""
    data = get_children(token, subpath)
    entries = data.get("value", [])
    while "@odata.nextLink" in data:  # handle pagination
        data = get_next(token, data)
        entries.extend(data.get("value", []))
    return entries

def list_children(token, subpath=""):
    """List files/subfolders in root or under SUBPATH.
    Adapted from https://www.merge.dev/blog/onedrive-api-python"""
    entries = get_all_children(token, subpath)
    for entry in entries:
        what = "Folder" if "folder" in entry else "File"
        print(f"- {what}: {entry['name']} (ID: {entry['id']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
```
Example output:
```sh
# list_children(token)
- Folder: Attachments (ID: [..], Last Modified: 2025-03-05T05:46:49Z)
- Folder: backups (ID: [..], Last Modified: 2026-01-15T01:20:28Z)
- File: Coffre-fort (ID: [..], Last Modified: 2025-02-02T11:36:40Z)
- Folder: Documents (ID: [..], Last Modified: 2025-01-27T12:02:00Z)
- Folder: Images (ID: [..], Last Modified: 2025-10-02T20:13:39Z)
# I don't know if it's a risk to reveal the IDs
```
I don't know if versions could be paginated, but since the structure is so similar, I wrote analogous functions for it:
```python
def get_versions(token, itemid):
    """Data with versions of a file with given ITEMID."""
    return get_me(token, f"/items/{itemid}/versions")

def get_all_versions(token, itemid):
    """get_versions wrapper with pagination handling."""
    data = get_versions(token, itemid)
    entries = data.get("value", [])
    while "@odata.nextLink" in data:  # handle pagination
        data = get_next(token, data)
        entries.extend(data.get("value", []))
    return entries

def list_versions(token, itemid):
    """List versions for file with given itemid."""
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"No versions found for {itemid}")
        return
    for entry in entries:
        print(f"- {entry['id']} (ID) (Size: {entry['size']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
```
There is no endpoint for listing the versions of a file by path. We can get file information by path and combine it like this:
```python
def get_file_info(token, path):
    """Get information object for file at PATH.
    (Path relative to root, without starting or ending /)"""
    return get_me(token, f"/root:/{path}")

def get_itemid(token, path):
    """Get itemid of file at PATH.
    (Path relative to root, without starting or ending /)"""
    data = get_file_info(token, path)
    return data["id"]

def list_path_versions(token, path):
    """list_versions convenience wrapper to be able to give a path
    instead of itemid.
    (Path relative to root, without starting or ending /)"""
    list_versions(token, get_itemid(token, path))
```
Example output:
```sh
# list_path_versions(token, "Documents/archupgrade.txt")
- 52.0 (ID) (Size: 32880, Last Modified: 2026-02-23T02:15:43Z)
- 49.0 (ID) (Size: 44040, Last Modified: 2026-01-15T23:09:34Z)
- 47.0 (ID) (Size: 23643, Last Modified: 2025-12-20T11:29:30Z)
- 45.0 (ID) (Size: 45158, Last Modified: 2025-12-07T14:15:25Z)
- 43.0 (ID) (Size: 18215, Last Modified: 2025-10-28T08:19:54Z)
- 39.0 (ID) (Size: 41976, Last Modified: 2025-10-20T12:46:24Z)
- 33.0 (ID) (Size: 346773, Last Modified: 2025-09-18T05:59:07Z)
- 29.0 (ID) (Size: 31485, Last Modified: 2025-07-22T02:04:16Z)
- 23.0 (ID) (Size: 21696, Last Modified: 2025-05-29T21:36:51Z)
- 19.0 (ID) (Size: 47418, Last Modified: 2025-05-09T08:17:34Z)
- 11.0 (ID) (Size: 20462, Last Modified: 2025-02-27T04:49:37Z)
- 1.0 (ID) (Size: 288, Last Modified: 2025-02-27T03:41:00Z)
```

Structure of information from `get_file_info`:
```json
{
  "@odata.context": "[..]",
  "@microsoft.graph.downloadUrl": "[..]",
  "createdDateTime": "2025-01-27T15:38:22Z",
  "eTag": "[..]",
  "id": "[..]",
  "lastModifiedDateTime": "2025-01-27T15:44:54Z",
  "name": "tokyo-expressway.mp3",
  "webUrl": "https://onedrive.live.com?cid=[..]",
  "cTag": "[..]",
  "media": {
    "aboutVisibility": "all",
    "analyticsVisibility": "all",
    "chatVisibility": "all",
    "interactivity": {"isInteractiveContentShown": true},
    "isNoiseSuppressionControlShown": true,
    "isWatermarkEnabled": false,
    "noiseSuppressionEnabledByDefault": false,
    "notesVisibility": "all",
    "tableOfContentsVisibility": "none",
    "viewpoint": {
      "areReactionsAllowed": true,
      "isAutomaticTranscriptionAllowed": true,
      "isTranscriptionAllowed": true,
      "isTranscriptionTranslationAllowed": false
    }
  },
  "size": 572902165,
  "createdBy": {"user": {"email": "[..]", "id": "[..]", "displayName": "[..]"}},
  "lastModifiedBy": {"user": {"email": "[..]", "id": "[..]", "displayName": "[..]"}},
  "parentReference": {
    "driveType": "personal",
    "driveId": "[..]",
    "id": "[..]",
    "name": "mus",
    "path": "/drive/root:/mus",
    "siteId": "[..]"
  },
  "file": {
    "mimeType": "audio/mpeg",
    "hashes": {
      "quickXorHash": "[..]",
      "sha1Hash": "[..]",
      "sha256Hash": "[..]"
    }
  },
  "fileSystemInfo": {
    "createdDateTime": "2025-01-27T15:38:22Z",
    "lastModifiedDateTime": "2025-01-27T15:44:54Z"
  }
}
```

Structure of information of a single version entry from `get_all_children`:
- Same as above, just without "@odata.context"

Structure of information of a single version entry from `get_all_versions`:
```json
{
  "@microsoft.graph.downloadUrl": "[..]",
  "id": "52.0",
  "lastModifiedDateTime": "2026-02-23T02:15:43Z",
  "size": 32880,
  "lastModifiedBy": {
    "application": {"id": "[..]", "displayName": "[..]"},
    "user": {"email": "[..]", "displayName": "[..]"}
  }
}
```

## Downloading files
```python
def download_url(token, url, to):
    """Function for downloading a @microsoft.graph.downloadUrl URL."""
    r = requests.get(url)
    with open(to, 'wb') as f:
        f.write(get_res(r, json=False))
        print(f"Downloaded to '{to}'")

def download_file(token, path, to):
    """Download file at OneDrive PATH to local path TO.
    (PATH relative to the root of the drive, without starting or ending /)"""
    data = get_file_info(token, path)
    download_url(token, data["@microsoft.graph.downloadUrl"], to)
```

## Downloading particular versions
Get all versions and download the one that matches a given ID (e.g. `19.0`).
```python
def download_file_version(token, itemid, versionid, to):
    """Download version VERSIONID of item with given ITEMID to local path
    TO."""
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"No versions found for {itemid}")
        return
    for entry in entries:
        if entry["id"] == versionid:
            download_url(token, entry["@microsoft.graph.downloadUrl"], to)
            return
    raise RuntimeError(f"No version with ID {versionid} found")
```

To get a version by date we could use `dateutil.parser.parse` from the builtin module `dateutil` to get a date object from the user date input and from the `lastModifiedDateTime` on the version, and compare the two dates. The date strings the API returns have a timezone (Z, meaning UTC), so Python refuses to compare them with a date that does not have a timezone, so I coerce it to local timezone first if one isn't given.
```python
def download_file_version_before_date(token, itemid, date, to):
    """Download version before DATE of item with given ITEMID to local path
    TO. DATE should be in a format supported by dateutil.parser.parse, e.g.
    '2026-04-04 10:30'. If timezone is not provided it's assumed to be the
    local timezone."""
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"No versions found for {itemid}")
        return
    date = dateutil.parser.parse(date)
    if not date.tzinfo:    # can't compare timezone-aware with unaware
        date = date.astimezone()
    for entry in entries:
        if dateutil.parser.parse(entry["lastModifiedDateTime"]) < date:
            print(f"Downloading version {entry['id']} from",
                  entry["lastModifiedDateTime"])
            download_url(token, entry["@microsoft.graph.downloadUrl"], to)
            return
    raise RuntimeError(f"No version before date {date} found")
```
Maybe I should coerce it to UTC rather than local timezone, but it seems like there is no method for that on date objects. You can give `astimezone` an argument, but it has to be an instance of `tzinfo`, so we would have to import it.

Example input/output:
```sh
# download_file_version_before_date(token, get_itemid(token, "Documents/archupgrade.txt"), "2025-05-29", "/home/pm/.config/onedriveverrer/test")
Downloading version 19.0 from 2025-05-09T08:17:34Z
Downloaded to '/home/pm/.config/onedriveverrer/test'
```

## Polyvalent path/itemid functions
Like we did with `list_path_versions` and `download_path_version`, I would like to also have a version that takes a path instead of an itemid, but I don't want to to continue to have two versions of each function. The names for the path variants are also a bit contrived. To simplify the API it would be nice to be able to pass either a path or an itemid. Maybe the way to do this is require a `/` at the beginning of the string for paths, seeing as the IDs can't have slashes in them.

{% include note.html content='
> [!NOTE]
> The format for item IDs seems to be the drive ID (A-Z0-9) followed by ! followed by another hash that is unique to each item (a-z0-9).
' %}

```python
def list_versions(token, of):
    """List versions for file with given itemid or path.
    (Path relative to root, with starting /)"""
    is_path = of[0] == "/"
    itemid = get_itemid(token, of) if is_path else of
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"No versions found for {itemid}")
        return
    for entry in entries:
        print(f"- {entry['id']} (ID) (Size: {entry['size']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
```
```python
def download_file_version(token, of, versionid, to):
    """Download version VERSIONID of item with given itemid or path OF
    to local path TO.
    (Path relative to root, with starting /)"""
    is_path = of[0] == "/"
    itemid = get_itemid(token, of) if is_path else of
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"No versions found for {itemid}")
        return
    for entry in entries:
        if entry["id"] == versionid:
            download_url(token, entry["@microsoft.graph.downloadUrl"], to)
            return
    raise RuntimeError(f"No version with ID {versionid} found")
```
```python
def download_file_version_before_date(token, of, date, to):
    """Download version before DATE of item with given itemid or path OF to
    local path TO. DATE should be in a format supported by
    dateutil.parser.parse, e.g. '2026-04-04 10:30'. If timezone is not
    provided it's assumed to be the local timezone."""
    is_path = of[0] == "/"
    itemid = get_itemid(token, of) if is_path else of
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"No versions found for {itemid}")
        return
    date = dateutil.parser.parse(date)
    if not date.tzinfo:    # can't compare timezone-aware with unaware
        date = date.astimezone()
    for entry in entries:
        if dateutil.parser.parse(entry["lastModifiedDateTime"]) < date:
            print(f"Downloading version {entry['id']} from",
                  entry["lastModifiedDateTime"])
            download_url(token, entry["@microsoft.graph.downloadUrl"], to)
            return
    raise RuntimeError(f"No version before date {date} found")
```

I will also modify everything else to expect paths starting with `/` for consistency.

## Single function to download file version by id or date
We could also actually combine `download_file_version` and `download_file_version_before_date` into a single function; assuming version IDs are always in the format `[0-9]+\.0` (e.g. 1.0, 11.0, 19.0), and no date can be expressed in this way, we could figure out which one it is.
```python
def find_versionid_in_entries(token, versionid, entries):
    """Return entry with version VERSIONID in entries array ENTRIES."""
    for entry in entries:
        if entry["id"] == versionid:
            return entry
    raise RuntimeError(f"No version with ID {versionid} found")

def find_version_before_date_in_entries(token, date, entries):
    """Return entry before DATE in entries array ENTRIES.
    (Date in a format supported by dateutil.parser.parse, e.g.
    '2026-04-04 10:30'. If timezone is not provided it's assumed to be the
    local timezone.)"""
    date = dateutil.parser.parse(date)
    if not date.tzinfo:  # can't compare timezone-aware with unaware
        date = date.astimezone()
    for entry in entries:
        if dateutil.parser.parse(entry["lastModifiedDateTime"]) < date:
            return entry
    raise RuntimeError(f"No version before date {date} found")

def download_file_version(token, of, ver, to):
    """Download version VER of item with given itemid or path OF
    to local path TO.
    VER can be either the version id or a date which the version is older than
    (downloads version before date).
    (Path relative to root, with starting /)
    (Date in a format supported by dateutil.parser.parse, e.g.
    '2026-04-04 10:30'. If timezone is not provided it's assumed to be the
    local timezone.)"""
    is_path = of[0] == "/"
    itemid = get_itemid(token, of) if is_path else of
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"No versions found for {itemid}")
        return
    ver_is_date = re.match(r"[0-9]+\.0", ver) is None
    entry = find_version_before_date_in_entries(token, ver, entries) \
        if ver_is_date \
        else find_versionid_in_entries(token, ver, entries)
    print(f"Downloading version {entry['id']} from",
          entry["lastModifiedDateTime"])
    download_url(token, entry["@microsoft.graph.downloadUrl"], to)
```

I realise we started with 2 functions and now we have 3, but it's a single function for downloading file version, I just separated concerns a bit.

## Combine into just one download file function
I guess since `download_file` is a very small and basic function, we could add the logic of `download_file_version` into it with optional `ver` to have just a single download file interface.

Also:
- Rename arguments `OF` to `FILE` and `TO` to `DEST` which are more standard names.
- Verify we have been given a file and not a folder, otherwise downloading will fail (folders have no "@microsoft.graph.downloadUrl"). Maybe it's easier to just check for "@microsoft.graph.downloadUrl" then and advise that it isn't there for folders, since we can't tell whether we have been given a file or folder until we have the entry details anyway.

```python
def download_file(token, file, dest="", ver=None):
    """Download FILE given its id or path. It will be downloaded to pwd with
    the same name as on OneDrive unless a destination path DEST is provided.
    A versionid or date VER can be specified to download that version or most
    recent version before date.
    PATH should be relative to root and start with a /.
    DATE should be in a format supported by dateutil.parser.parse, e.g.
    '2026-04-04 10:30'. If timezone is not provided it's assumed to be
    the local timezone."""
    entry = None
    if not ver:
        entry = get_file_info(token, file)
    else:
        is_path = file[0] == "/"
        itemid = get_itemid(token, file) if is_path else file
        entries = get_all_versions(token, itemid)
        if not entries:
            print(f"No versions found for {itemid}")
            return
        ver_is_date = re.match(r"[0-9]+\.0", ver) is None
        entry = find_version_before_date_in_entries(token, ver, entries) \
            if ver_is_date \
            else find_versionid_in_entries(token, ver, entries)
    if not len(dest):
        dest = entry["name"]
    if "@microsoft.graph.downloadUrl" not in entry:
        raise RuntimeError("No '@microsoft.graph.downloadUrl' in entry. This \
can happen if the path provided is a folder instead of a file.")
    what = f"version {entry['id']}" if ver else f"file {entry['name']}"
    print(f"Downloading {what} (Last Modified: "
          f"{entry['lastModifiedDateTime']}) to {dest}")
    download_url(token, entry["@microsoft.graph.downloadUrl"], dest)
```

## Avoid overwriting an existing file by the same name
Currently our download file function will ruthlessly crush an existing local file, if one already exists in DEST. This can be easily fixed by changing the flags we give [`open`](https://docs.python.org/3/library/functions.html#open) from "wb" to "xb" (x for exclusive creation, b binary mode).

```python
def download_url(token, url, dest, overwrite=False):
    """Function for downloading a @microsoft.graph.downloadUrl URL."""
    r = requests.get(url)
    flags = "wb" if overwrite else "xb"
    with open(to, flags) as f:
        f.write(get_res(r, json=False))
        print(f"Downloaded to '{to}'")
```

## Listing children recursively
What I'm building up to is downloading a "version" of a folder. The next step then is to be able to get not just the immediate children, but the entire tree under a particular path.

In a display of questionable priorities, I first modified it to give a directory-tree sort of output:
```python
def list_children(token, path=""):
    """List files/subfolders in root or under PATH."""
    entries = get_all_children(token, path)
    for i, entry in enumerate(entries):
        pre = "└──" if i == len(entries)-1 else "├──"
        is_folder = "folder" in entry
        what = "+" if is_folder else ""
        print(f"{pre}{what} {entry['name']} (ID: {entry['id']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
```
→
```sh
# list_children(token)
├──+ Attachments ([..])
├──+ backups ([..])
├── Coffre-fort ([..])
├──+ Documents ([..])
└──+ Images ([..])
```
and now for the folders, we could make this function recursive and track the level of recursion
```python
def list_children(token, path="", recurse=False, rlevel=0):
    """List files/subfolders in root or under PATH.
    If RECURSE, list descendants. RLEVEL tracks depth."""
    entries = get_all_children(token, path)
    for i, entry in enumerate(entries):
        pre = "└──" if i == len(entries)-1 else "├──"
        pre = "│   " * rlevel + pre
        is_folder = "folder" in entry
        what = "+" if is_folder else ""
        print(f"{pre}{what} {entry['name']} (ID: {entry['id']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
        if recurse and is_folder:
            p = f"{path}/{entry['name']}" if path else entry['name']
            list_children(token, p, True, rlevel+1)
```
→
```sh
# list_children(token, "backups/reps/plu5.github.io/_notes", True)
├──+ k ([..])
│   ├── firefox.md ([..])
│   └── linux.md ([..])
├──+ n ([..])
│   ├── bmqs.md ([..])
│   └── hansard.md ([..])
├──+ pers ([..])
│   ├──+ proj ([..])
│   │   ├── mwin.md ([..])
│   │   └── retype.md ([..])
│   ├── art-ascii.md ([..])
│   ├── en-vrac.md ([..])
│   ├── good-films.md ([..])
│   └── traces-media.md ([..])
└──+ s ([..])
│   └──+ l ([..])
│   │   └── phrases-fr.md ([..])
```
To avoid those excess vertical lines, instead of `rlevel` we need to pass a prefix, adding to it a vertical line or not depending on if we're on the last item. The "connector" needs to be kept separate, not in `pre`, because the children are not supposed to have it.
```python
def list_children(token, path="", recurse=False, pre=""):
    """List files/subfolders in root or under PATH.
    If RECURSE, list descendants. PRE is an extra prefix before each item."""
    entries = get_all_children(token, path)
    for i, entry in enumerate(entries):
        is_last = i == len(entries)-1
        connector = "└──" if is_last else "├──"
        is_folder = "folder" in entry
        what = "+" if is_folder else " "
        print(f"{pre}{connector}{what}{entry['name']} (ID: {entry['id']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
        if recurse and is_folder:
            p = f"{path}/{entry['name']}" if path else entry['name']
            pre_ = pre + ("    " if is_last else "│   ")
            list_children(token, p, True, pre_)
```
I also removed the space between `{what}` and `{entry['name']}` in order for the children to be aligned right under the folder name.
```sh
# list_children(token, "backups/reps/plu5.github.io/_notes", True)
├──+k ([..])
│   ├── firefox.md ([..])
│   └── linux.md ([..])
├──+n ([..])
│   ├── bmqs.md ([..])
│   └── hansard.md ([..])
├──+pers ([..])
│   ├──+proj ([..])
│   │   ├── mwin.md ([..])
│   │   └── retype.md ([..])
│   ├── art-ascii.md ([..])
│   ├── en-vrac.md ([..])
│   ├── good-films.md ([..])
│   └── traces-media.md ([..])
└──+s ([..])
    └──+l ([..])
        └── phrases-fr.md ([..])
```

## Reconstruct a folder as it was before a given date
The next step I guess is, given a date, to call `download_file_version_before_date` for each file. For now I will comment out the call to `download_url` so that it doesn't actually download (or add an argument `dry_run`), and we'll just see if it is capable to get a good version for each one. To do this, let's modify `list_children` to be able to take a callback and call it on each descendant:
```python
def list_children(token, path="", recurse=False, pre="", callback=None):
    """List files/subfolders in root or under PATH.
    If RECURSE, list descendants. PRE is an extra prefix before each item.
    If CALLBACK is provided, it will be called with info of each descendant."""
    entries = get_all_children(token, path)
    for i, entry in enumerate(entries):
        is_last = i == len(entries)-1
        connector = "└──" if is_last else "├──"
        is_folder = "folder" in entry
        what = "+" if is_folder else " "
        print(f"{pre}{connector}{what}{entry['name']} (ID: {entry['id']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
        if (callback):
            callback(entry)
        if recurse and is_folder:
            p = f"{path}/{entry['name']}" if path else entry['name']
            pre_ = pre + ("    " if is_last else "│   ")
            list_children(token, p, True, pre_, callback)
```

```python
def download_folder(token, path, dest="", ver=None, dry_run=False):
    def cb(entry):
        p = f'{entry["parentReference"]["path"].split(":")[1]}/{entry["name"]}'
        dest_ = (dest if dest else ".") + p
        if "folder" in entry:
            print(f"Creating folder {dest_}")
            if not dry_run:
                makedirs(dest_)
        else:
            download_file(token, p, dest_, ver, dry_run=dry_run)
    list_children(token, path, True, callback=cb)
```
It works well, but if we download for example "/backups/reps/plu5.github.io/.git" it creates this entire path in dest, not just .git.

By the way a weird thing here is we can't change the value of `dest` inside `cb` because of python scoping rules, unless we explicitly declare it `nonlocal`. I just put it in a different variable (`dest_`).

Get the entry of the folder first, then we can remove its path from the child paths:
```python
def download_folder(token, path, dest="", ver=None, dry_run=False):
    entry = get_file_info(token, path)
    if "folder" not in entry:
        raise ValueError(f"{path} is not a folder")
    dest = dest if dest else entry["name"]
    top = f'{entry["parentReference"]["path"].split(":")[1]}/{entry["name"]}'

    def cb(entry):
        p = f'{entry["parentReference"]["path"].split(":")[1]}/{entry["name"]}'
        dest_ = dest + (p[len(top):] if p.startswith(top) else p)
        if "folder" in entry:
            print(f"Creating folder {dest_}")
            if not dry_run:
                makedirs(dest_)
        else:
            download_file(token, p, dest_, ver, dry_run=dry_run)

    list_children(token, path, True, callback=cb)
```

It breaks if there is a file that doesn't have a version before the date. I suppose that's not invalid because there could be files that didn't exist yet.

Instead of raising an exception in `find_version_before_date_in_entries` and `find_versionid_in_entries` just print, and in `download_file` in the version path, return empty handed if these functions didn't return an entry
```python
        entry = find_version_before_date_in_entries(token, ver, entries) \
            if ver_is_date \
            else find_versionid_in_entries(token, ver, entries)
        if not entry:
            return
```

## onedriveverrer
I called my script onedriveverrer, and here it is in full (as usual, replace `CLIENTID` and `USERAGENT`):
```python
#!/usr/bin/env python3
# OneDrive verrer
# 2026-04-02 13:24
"""Script to download an older version of a OneDrive file or folder.

Examples:
    onedriveverrer --list-children ''
    onedriveverrer --download-file '/test.txt'  # current version
    onedriveverrer --download-file '/test.txt' 2026-03-01 ~/test.txt
    onedriveverrer --download-folder '/test' 2026-03-01 ~/test
"""

import re  # re.search for extracting auth code from url, re.match versionid
import sys                      # sys.argv
import json                     # load and dump in get_tokens and save_tokens
import argparse                 # CLI
import requests                 # all communication with server
import dateutil                 # dateutil.parser for version_before_date
from time import time           # tokens expiration calculation
from os import makedirs         # save_tokens, download_folder
from os.path import (
    expanduser,                 # expand ~
    dirname,                    # with makedirs in save_tokens
    getmtime)                   # tokens expiration calculation

NAME = "onedriveverrer"
DESCRIPTION = "Download an older version of a OneDrive file or folder."
ARGS = [(["--auth"], {"action": "store_true", "help": "\
Use our own refresh token or do an auth flow to generate one instead of using \
the assumed existing refresh token of a OneDrive client in \
~/.config/onedrive/refresh_token."}),
        (["--dry-run"], {"action": "store_true", "help": "\
With download operations, only log without downloading."}),
        (["--dump"], {"help": "\
Get file information object for given PATH, which should be relative to root \
and start with a /.", "metavar": "PATH"}),
        (["-l", "--list-children"], {"help": "\
List files and subfolders under given PATH, which should be relative to root \
and start with a /. Empty string will list top-level.", "metavar": "PATH"}),
        (["-a", "--list-arborescence"], {"help": "\
Like --list-children but recursive.", "metavar": "PATH"}),
        (["--list-versions"], {"help": "\
List versions of a file given its ID or PATH. PATH should be relative to \
root and start with a /. ID should be passed in single quotes in shells like \
Bash or it will try to interpret the ! in it.", "metavar": "(ID|PATH)"}),
        (["--download-file"], {"nargs": "+", "help": "\
Download a file given its ID or PATH. It will be downloaded to pwd with the \
same name as on OneDrive unless a destination path is provided as second \
argument DEST. A VERSIONID or DATE can be specified as the third argument to \
download the version specified or most recent version before DATE. \
PATH should be relative to root and start with a /. \
DATE should be in a format supported by dateutil.parser.parse, e.g. \
'2026-04-04 10:30'. If timezone is not provided it's assumed to be the local \
timezone.", "metavar": "(ID|PATH) [DEST] [VERSIONID|DATE]"}),
        (["--download-folder"], {"nargs": "+", "help": "\
Download a folder given its PATH. It will be downloaded to pwd with the \
same name as on OneDrive unless a destination path is provided as second \
argument DEST. A DATE can be specified as the third argument to \
download for each file the most recent version before DATE. \
PATH should be relative to root and start with a /. \
DATE should be in a format supported by dateutil.parser.parse, e.g. \
'2026-04-04 10:30'. If timezone is not provided it's assumed to be the local \
timezone.", "metavar": "PATH [DEST] [DATE]"})]

# Used if no --auth
EXISTINGRTOKENPATH = expanduser("~/.config/onedrive/refresh_token")  # expand ~
# Used always (if nothing else, to store the access token)
OWNTOKENSPATH = expanduser("~/.config/onedriveverrer/tokens")

CLIENTID = "[..client id..]"
USERAGENT = "[..user agent..]"
SCOPE = "\
Files.ReadWrite Files.ReadWrite.All Sites.ReadWrite.All offline_access"
AUTHDOMAIN = "https://login.microsoftonline.com/common/oauth2"
REDIRECTURI = f"{AUTHDOMAIN}/nativeclient"
AUTHURI = f"{AUTHDOMAIN}/v2.0/authorize"
TOKENURI = f"{AUTHDOMAIN}/v2.0/token"
GRAPHDOMAIN = "https://graph.microsoft.com/v1.0/me/drive"


def get_res(r, json=True):
    """Get json response of a request or print details and raise exception
    if it failed."""
    if r.status_code == 200:
        return r.json() if json else r.content
    else:
        try:
            print(r.json())
        except requests.exceptions.JSONDecodeError:
            print(f"{NAME}: No json body. Status code: {r.status_code}")
        r.raise_for_status()


def headers(token=None):
    """Return standard headers to send with requests."""
    if token:
        return {"authorization": f"bearer {token}", "user-agent": USERAGENT}
    return {"user-agent": USERAGENT}


def auth():
    """Interactive auth flow to generate a refresh token.
    Requires opening a page in the browser, logging in, and pasting the URL of
    the page one got redirected to, which should contain ?code= parameter."""
    print("Please authorise this application by visiting the following URL:")
    d = {"client_id": CLIENTID,
         "redirect_uri": REDIRECTURI,
         "scope": SCOPE.replace(" ", "%20"),
         "response_type": "code",
         "prompt": "login"}
    print(AUTHURI + "?" + "&".join("%s=%s" % (k, v) for k, v in d.items()))
    print("After completing the authorisation in your browser, copy the full "
          "redirect URI (from the address bar) and paste it below.")
    response = input("Paste redirect URI here: ")
    code = re.search(r"(?:[?&]code=)([^&]+)", response.strip())
    if not code:
        raise RuntimeError("An empty or invalid redirect URI was entered")
    d = {"client_id": CLIENTID,
         "redirect_uri": REDIRECTURI,
         "code": code.group(1),
         "grant_type": "authorization_code"}
    r = requests.post(TOKENURI, headers=headers(),
                      data="&".join("%s=%s" % (k, v) for k, v in d.items()))
    return get_res(r)


def save_tokens(tokens, path):
    """Save object TOKENS at PATH, creating containing directories if
    necessary."""
    makedirs(dirname(path), exist_ok=True)
    with open(path, "w") as f:
        json.dump(tokens, f)
        print(f"{NAME}: Wrote tokens in {path}")


def get_tokens(path, arg_auth=False):
    """Get tokens from file at PATH.
    If arg_auth is True and there are no tokens on disk, trigger
    interactive auth flow to generate them."""
    tokens = None
    try:
        with open(path, "r") as f:
            tokens = json.load(f)
    except json.decoder.JSONDecodeError as e:
        print(f"{NAME}: Something wrong with existing tokens file {path}: {e}")
        print(f"{NAME}: Will attempt to regenerate tokens.")
    except FileNotFoundError:
        print(f"{NAME}: No tokens in {path}. "
              "This is normal if we have not generated any yet.")
        if arg_auth:            # FIXME: Not tested this codepath
            print(f"{NAME}: Triggering interactive auth flow...")
            tokens = auth()
            save_tokens(tokens, path)
    return tokens


def read_refreshtoken(path):
    """Get refresh token from file at PATH.
    This is only used if --auth is not provided which means we are using an
    existing refresh token generated by another application. It must exist or
    it's a fatal error."""
    refreshtoken = None
    try:
        with open(path, "r") as f:
            refreshtoken = f.read()
            if not refreshtoken:
                raise RuntimeError(f"Empty refresh token file in {path}")
    except FileNotFoundError:
        raise RuntimeError(f"No refresh token file in {path}")
    return refreshtoken


def fetch_tokens(refreshtoken):
    """Fetch new tokens using a refresh token."""
    d = {"client_id": CLIENTID,
         "redirect_uri": REDIRECTURI,
         "refresh_token": refreshtoken,
         "grant_type": "refresh_token"}
    r = requests.post(TOKENURI, headers=headers(), data=d)
    return get_res(r)


def get_me(token, sub=""):
    """Drive details."""
    r = requests.get(f"{GRAPHDOMAIN}{sub}", headers=headers(token))
    return get_res(r)


def get_file_info(token, path):
    """Get information object for file at PATH.
    (Path relative to root, with starting /)"""
    return get_me(token, f"/root:{path}")


def get_itemid(token, path):
    """Get itemid of file at PATH.
    (Path relative to root, with starting /)"""
    data = get_file_info(token, path)
    return data["id"]


def get_children(token, path=""):
    """Drive top-level files, or files in PATH.
    (Path relative to root, with starting /)"""
    if path and path[0] != "/":
        raise ValueError("PATH must either be empty or begin with a /")
    p = f"/root:{path}:/children" if path else "/root/children"
    return get_me(token, p)


def get_next(token, data):
    """Resolve next link in data. Used for pagination."""
    r = requests.get(data['@odata.nextLink'], headers=headers(token))
    return get_res(r)


def get_all_children(token, path=""):
    """get_children wrapper with pagination handling."""
    data = get_children(token, path)
    entries = data.get("value", [])
    while "@odata.nextLink" in data:  # handle pagination
        data = get_next(token, data)
        entries.extend(data.get("value", []))
    return entries


def list_children(token, path="", recurse=False, pre="", callback=None):
    """List files/subfolders in root or under PATH.
    If RECURSE, list descendants. PRE is an extra prefix before each item.
    If CALLBACK is provided, it will be called with info of each descendant."""
    entries = get_all_children(token, path)
    for i, entry in enumerate(entries):
        is_last = i == len(entries)-1
        connector = "└──" if is_last else "├──"
        is_folder = "folder" in entry
        what = "+" if is_folder else " "
        print(f"{pre}{connector}{what}{entry['name']} (ID: {entry['id']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")
        if (callback):
            callback(entry)
        if recurse and is_folder:
            p = f"{path}/{entry['name']}" if path else entry['name']
            pre_ = pre + ("    " if is_last else "│   ")
            list_children(token, p, True, pre_, callback)


def get_versions(token, itemid):
    """Data with versions of a file with given ITEMID."""
    return get_me(token, f"/items/{itemid}/versions")


def get_all_versions(token, itemid):
    """get_versions wrapper with pagination handling."""
    data = get_versions(token, itemid)
    entries = data.get("value", [])
    while "@odata.nextLink" in data:  # handle pagination
        data = get_next(token, data)
        entries.extend(data.get("value", []))
    return entries


def list_versions(token, of):
    """List versions for file with given itemid or path.
    (Path relative to root, with starting /)"""
    is_path = of[0] == "/"
    itemid = get_itemid(token, of) if is_path else of
    entries = get_all_versions(token, itemid)
    if not entries:
        print(f"{NAME}: No versions found for {itemid}")
        return
    for entry in entries:
        print(f"- {entry['id']} (ID) (Size: {entry['size']}, "
              f"Last Modified: {entry['lastModifiedDateTime']})")


def download_url(token, url, dest, overwrite=False):
    """Function for downloading a @microsoft.graph.downloadUrl URL."""
    r = requests.get(url)
    flags = "wb" if overwrite else "xb"
    with open(dest, flags) as f:
        f.write(get_res(r, json=False))
        print(f"{NAME}: Downloaded to '{dest}'")


def find_versionid_in_entries(token, versionid, entries):
    """Return entry with version VERSIONID in entries array ENTRIES."""
    for entry in entries:
        if entry["id"] == versionid:
            return entry
    print(f"{NAME}: No version with ID {versionid} found")


def find_version_before_date_in_entries(token, date, entries):
    """Return entry before DATE in entries array ENTRIES.
    (Date in a format supported by dateutil.parser.parse, e.g.
    '2026-04-04 10:30'. If timezone is not provided it's assumed to be the
    local timezone.)"""
    date = dateutil.parser.parse(date)
    if not date.tzinfo:  # can't compare timezone-aware with unaware
        date = date.astimezone()
    for entry in entries:
        if dateutil.parser.parse(entry["lastModifiedDateTime"]) < date:
            return entry
    print(f"{NAME}: No version before date {date} found")


def download_file(token, file, dest="", ver=None, dry_run=False):
    """Download FILE given its id or path. It will be downloaded to pwd with
    the same name as on OneDrive unless a destination path DEST is provided.
    A versionid or date VER can be specified to download that version or most
    recent version before date.
    PATH should be relative to root and start with a /.
    DATE should be in a format supported by dateutil.parser.parse, e.g.
    '2026-04-04 10:30'. If timezone is not provided it's assumed to be
    the local timezone."""
    entry = None
    if not ver:
        entry = get_file_info(token, file)
    else:
        is_path = file[0] == "/"
        itemid = get_itemid(token, file) if is_path else file
        entries = get_all_versions(token, itemid)
        if not entries:
            print(f"{NAME}: No versions found for {itemid}")
            return
        ver_is_date = re.match(r"[0-9]+\.0", ver) is None
        entry = find_version_before_date_in_entries(token, ver, entries) \
            if ver_is_date \
            else find_versionid_in_entries(token, ver, entries)
        if not entry:
            return
    if not len(dest):
        dest = entry["name"]
    if "@microsoft.graph.downloadUrl" not in entry:
        raise RuntimeError("No '@microsoft.graph.downloadUrl' in entry. This \
can happen if the path provided is a folder instead of a file.")
    what = f"version {entry['id']}" if ver else f"file {entry['name']}"
    print(f"{NAME}: Downloading {what} (Last Modified: "
          f"{entry['lastModifiedDateTime']}) to {dest}")
    if not dry_run:
        download_url(token, entry["@microsoft.graph.downloadUrl"], dest)


def download_folder(token, path, dest="", ver=None, dry_run=False):
    """Download folder given its PATH. It will be downloaded to pwd with
    the same name as on OneDrive unless a destination path DEST is provided.
    A date VER can be specified to download for each file the most recent
    version before given date.
    PATH should be relative to root and start with a /.
    DATE should be in a format supported by dateutil.parser.parse, e.g.
    '2026-04-04 10:30'. If timezone is not provided it's assumed to be
    the local timezone."""
    entry = get_file_info(token, path)
    if "folder" not in entry:
        raise ValueError(f"{path} is not a folder")
    dest = dest if dest else entry["name"]
    top = f'{entry["parentReference"]["path"].split(":")[1]}/{entry["name"]}'

    def cb(entry):
        p = f'{entry["parentReference"]["path"].split(":")[1]}/{entry["name"]}'
        dest_ = dest + (p[len(top):] if p.startswith(top) else p)
        if "folder" in entry:
            print(f"{NAME}: Creating folder {dest_}")
            if not dry_run:
                makedirs(dest_)
        else:
            download_file(token, p, dest_, ver, dry_run=dry_run)

    list_children(token, path, True, callback=cb)


def parse_args():
    """CLI."""
    parser = argparse.ArgumentParser(
        prog=NAME,
        description=DESCRIPTION)
    parser.suggest_on_error = True
    for args, kwargs in ARGS:
        parser.add_argument(*args, **kwargs)
    # parse args / print help and quit if no args
    # (Primer https://stackoverflow.com/a/47440202/18396947)
    return parser.parse_args(sys.argv[1:] or ['--help'])


if __name__ == "__main__":
    args = parse_args()

    tokens = get_tokens(OWNTOKENSPATH, args.auth)
    accesstoken = refreshtoken = None
    if not tokens:
        if args.auth:
            print(f"{NAME}: This should be unreachable, because when --auth "
                  "is provided, if getting tokens fails it does the "
                  "interactive auth flow, and if that fails an exception "
                  "would have kicked you out.")
            exit(1)
        else:
            refreshtoken = read_refreshtoken(EXISTINGRTOKENPATH)
            tokens = fetch_tokens(refreshtoken)
            save_tokens(tokens, OWNTOKENSPATH)
    else:
        if time() > getmtime(OWNTOKENSPATH) + tokens["expires_in"]:
            # CONCERN: Maybe it should be fetched with the refresh
            # token in EXISTINGRTOKENPATH if not args.auth, in case
            # this refresh token is also too old
            tokens = fetch_tokens(tokens["refresh_token"])
            save_tokens(tokens, OWNTOKENSPATH)
    refreshtoken = tokens['refresh_token']
    token = tokens['access_token']

    dry_run = args.dry_run
    if args.dump is not None:
        print(get_file_info(token, args.dump))
    if args.list_children is not None:
        list_children(token, args.list_children)
    if args.list_arborescence is not None:
        list_children(token, args.list_arborescence, True)
    if args.list_versions is not None:
        list_versions(token, args.list_versions)
    if args.download_file is not None:
        download_file(token, *args.download_file, dry_run=dry_run)
    if args.download_folder is not None:
        download_folder(token, *args.download_folder, dry_run=dry_run)
```

[version control link where I will put future changes](https://github.com/plu5/dotfiles/blob/main/pm/scripts/onedriveverrer)
(and there the client ID and user agent are not excerpted so it should work as is. don't abuse pls)

To maybe do:
- Add rate limit / throttling (429) handling (not encountered it but I am not checking the return headers currently and I perhaps should)
- More normal CLI interface, it's currently a bit eccentric
  ```sh
  # Current interface:
  # List children
  onedriveverrer -l "/path"
  onedriveverrer --list-children "/path"
  # List children recursive
  onedriveverrer --a "/path"
  onedriveverrer --list-arborescence "/path"
  # Download file/folder           (PATH DEST DATE) (dest and date optional)
  onedriveverrer --download-file "/path"
  onedriveverrer --download-file "/path" "~/here"
  onedriveverrer --download-file "/path" "" "2026-02-22"
  onedriveverrer --download-folder "/path" "" "2026-02-22"
  
  # More standard would be something like:
  onedriveverrer ls     # list children in root
  onedriveverrer ls -r  # list children recursive
  onedriveverrer download "/path" --dest "~/dest" --date "2026-02-22"
  # and this will allow to download a version from a date for example without
  # having to specify dest (not even as an empty string)
  onedriveverrer download "/path" --date "2026-02-22"
  ```

[merge.dev]: https://www.merge.dev/blog/onedrive-api-python
[skilion]: https://github.com/skilion/onedrive/
[abraunegg]: https://github.com/abraunegg/onedrive/
[rclone]: https://github.com/rclone/rclone/tree/master/backend/onedrive

{% include fin.html %}
