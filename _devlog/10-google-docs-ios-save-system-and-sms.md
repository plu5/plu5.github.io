---
layout: post
title: 10 — Google Docs iOS save system and SMS
date: 2026-03-01 20:14
modified_date: 2026-03-08 07:58
categories: google docs ios sms sql sqlite
lang: en
redirect_from: /devlog/10
---

It's not much of a devlog today. Misfortune and my attempts at dealing with it, but there is not a good outcome.

but I learned something, which then helped me in section 2.

## Google Docs iOS offline data reversion and unfruitful attempts at recovery
### A very small tragedy
Always been paranoid when editing a Google Docs document offline that when I come back online it is going to revert the changes. (If it's something that already happened to me before I no longer remember it.)

Yesterday I had many periods without internet connection (don't look at the news), and the first two times I returned to a network I was sufficiently paranoid to first both copy over the contents elsewhere and take screenshots, _au cas où_. but after the first few reconnections went without a hitch and no loss of data, I started to trust it and stopped taking extra care when returning. By the end of the day I had come to regret this; a note I had written had reverted itself upon reconnection, just as I feared might happen. but maybe the data is still there somewhere on disk?

### Paths
On iOS (13.5.1), there are two main locations applications store their data:
- /var/mobile/Containers/Data/Application ①
- /var/mobile/Containers/Shared/AppGroup ②

There is also /var/containers/Bundle/Application ③ where data from the ipa is extracted to when the application is installed, but that doesn't concern us as no user data is stored there.

The folder names are UUIDs. Even for the same application, the different data folders have different UUIDs. iOS filesystem browser software like Filza identifies them automatically, presumably by reading the contents.

I expect it's going to be different on each installation but I will note the UUIDs to make it easier for me to browse via ssh.
1. 1EEC8B78-8AC3-4680-9D64-184F6BA1E697
2. There is actually no Docs folder in ②. Possibly related: group.com.google.common (017BD2D4-4D50-4D9B-B96B-79C88BBBB28B), group.com.google.gsuite (BF5DFED6-129F-4721-9FF3-02E2C05CFC00)
3. 00B721E4-E1B0-4F4D-B300-1F4B9CA23484

### Detour looking at emoticons
As expected, there is no interesting information for us in ③. It's all static resources extracted from the ipa, dated 2022-05-23 19:02. Apart from the executable itself and the info.plist, dated 2022-05-24 05:24. but there's pretty cool art in there btw. and also a Very Important File `emoji_latest_ordering.json`, I'm sorry but I have to list at least some of them:
- `:D` :smile:
- `:-D` :smile-with-big-eyes:
- `^_^` :grin:
- `*^_^*` :grinning:
- `X-D` :laughing:
- `^_^;` :grin-sweat:
- `>w<` :joy:
- `*>w<*` :rofl:
- `;_;` :loudly-crying:
- `;)` :wink:
- `:*` :kissing:
- `^3^` :kissing-smiling-eyes:
- `:**` :kissing-closed-eyes:
- `;*` :kissing-heart:
- `<3:)` :heart-face: :3-hearts:
- `♥_♥` :heart-eyes:
- `*_*` :star-struck:
- `(:` :upside-down-face:
- `:)` `:-)` :slightly-happy:
- `:,)` :happy-cry:
- `:b` :yum:
- `:P` `:p` :stuck-out-tongue:
- `:-P` `:-p` :squinting-tongue:
- `;P` `;p` :winky-tongue:
- `o_O` :zany-face:
- `O:)` :halo:
- `>~>` :smirk:
- `._.` :pensive:
- `-_-` :expressionless:
- `:|` :neutral-face:
- `(・д・ゝ）` :salute:
- `=L` :thinking-face:
- `:‑X` :shushing-face:
- `~O~` :yawn:
- `\(^o^)/` :hug-face:
- `|д･)` :peeking:
- `@0@` :screaming:
- `(-_-)ゞ` :raised-eyebrow:
- `o~O` :monocle:
- `>->` :unamused:
- `X-(` :angry:
- `>:O` :rage:
- `#$@!` :cursing:
- `:S` :worried:
- `•_•'` :concerned:
- `:'(` :cry:
- `:-(` :big-frown:
- `:(` :frown:
- `:/` :diagonal-mouth:
- `:-/` :slightly-frowning:
- `:#` :zipper-face:
- `D-':` :anxious-with-sweat:
- `D-:` :scared:
- `D=` :gasp:
- `:O` :mouth-open:
- `:o` :surprised: :hushed:
- `8‑0` :flushed:
- `:-|` :grimacing:
- `(0へ0)` :sweat: :downcast:
- `>:[` :scrunched-mouth: :confounded: :zigzag-mouth:
- `>:(` :scrunched-eyes: :persevering:
- `D:` :weary:
- `D-X` :distraught:
- `X_o` :x-eyes:
- `Z_Z` :sleep: :tired:
- `^p^` :drool:
- `>_>` :moon-face-new:
- `<_<` :moon-face-full:
- `:-###` :sick: :nauseated:
- `:-O##` :vomit:
- `(*´台｀*)` :sneeze:
- `<):)` :cowboy:
- `B-)` :sunglasses-face:
- `:-B` :nerd-face:
- `:o)` :clown:
- `⊂(´・◡・⊂)∘˚˳°` :ghost:
- `༼^-^༽` :poop:
- `(<>..<>)` :alien:
- `└[∵┌]└[∵]┘[┐∵]┘` :robot:
- `3:)` :imp-smile:
- `3:(` :imp-frown:
- `:3` :smiley-cat:
- `<3` :red-heart:
- `<3<3` :two-hearts:
- `<3!` :heart-exclamation-point:
- `</3` :broken-heart:
- `♪┏(･o･)┛♪` :dancer-woman:
- `┗(･o･)┓♪` :dancer-man:
- `(-}{-)` :kiss-people:
- `@-,-'-,-` :rose:
- `=^.^=` :cat-face:
- `▼・ᴥ・▼` :dog-face:
- `ʕ·ᴥ·ʔ` :bear-face:
- `～>゜）～～～～` :snake:
- `<:3)~` :mouse:
- `(:3ꇤ⁐ꃳ` :otter:
- `⎛⎝(•ⱅ•)⎠⎞` :bat:

for those that contain escaped unicode characters `"\u239b\u239d(\u2022\u2c45\u2022)\u23a0\u239e"` like the bat, to convert to the real characters I used the Python interpreter, pasting the string in a `print()`.
```sh
$ python
Python 3.14.3 (main, Feb 13 2026, 15:31:44) [GCC 15.2.1
Type "help", "copyright", "credits" or "license" for mo
>>> print(          "\u239b\u239d(\u2022\u2c45\u2022)\\
u23a0\u239e")
⎛⎝(•ⱅ•)⎠⎞
```

### In ② (Shared/AppGroup)
In ②, as previously stated, there is no Docs folder.

group.com.google.common (017BD2D4-4D50-4D9B-B96B-79C88BBBB28B) contains cache

group.com.google.gsuite (BF5DFED6-129F-4721-9FF3-02E2C05CFC00) contains just empty folders

### In ① (Data/Application)
/var/mobile/Containers/Data/Application/1EEC8B78-8AC3-4680-9D64-184F6BA1E697

```sh
$ ls -a
./
../
.com.apple.mobile_container_manager.metadata.plist
Documents/
Library/
StoreKit/
SystemData/
tmp/
```

can ignore StoreKit which contains just receipt (2022-09-06 01:24)

and SystemData which contains just WebKit data (com.apple.SafariViewService) like cache, cookies, and localstorage for when you click a link in a document.

Library : preferences and more cache.

tmp has 3 subfolders:
1. (A Document Being Saved By Docs)
2. com.apple.dyld
3. WebKit

3 only contains empty folder MediaCache. 2 contains a .closure file with non-human-readable binary data, [something to do with app launch and dynamic linking](https://www.emergetools.com/glossary/dyld).

1 piqued my interest but I saw in /tmp other folders with similar names (`(A Document Being Saved by itunescloudd 2)`), so it's not something specific to Docs. Contains a binary file com.google.ar.core.ios.pb, and the date on it is 2026-02-12 00:52 so that rules it out in any case.

That only leaves Documents then. It contains two folders: my Google account numerical id (which I will henceforth refer to as `[guid]`) and a folder called drivekit.
```
 Documents/
 │
 ├── [guid]/
 │   ├── comments_snapshot_[guid].db
 │   ├── userFlags.json
 │   ├── fileStore/
 │   │   ├── documents/      † (1)
 │   │   └── globalFiles/
 │   │       ├── fonts/
 │   │       └── templates/
 │   │           └── thumbnail/
 │   └── localStore/
 │       ├── documents/      † (2)
 │       └── shared/
 │           ├── applicationMetadata.db
 │           ├── documentMetadata.db
 │           └── templateMetadata.db
 └── drivekit/
     └── users/
         └── [guid]/
             ├── cello/
             │   ├── cello.db
             │   └── cello.db_local_counter
             ├── files/
             └── thumbnails/ † (3)
```
Ignoring db-wal and db-shm files ([it's just temporary sqlite files](https://stackoverflow.com/questions/7778723/what-are-the-db-shm-and-db-wal-extensions-in-sqlite-databases)).

{% include note.html content='
> [!NOTE]
> Hang on a minute. satur9nine on the SE question I linked: "The contents of the WAL are periodically moved to the DB file but this is not guaranteed to occur each time the process exits. Thus when WAL is enabled each SQLite DB consists of two files on disk that must be preserved, both the .db file and the .db-wal file."
>
> Maybe I should not have ignored those files. All the documents do seem to have a db-wal (albeit empty), and maybe there would have been the missing data there? I always thought they were just garbage files. Welp, it is too late now, I had ignored it and after closing and reopening the app the database is rewritten, losing all the `document_commands`.
>
> Tested making a change to the document and it indeed does not appear in the db itself and the wal gets data written into it.
>
> However, I had verified the date on the the db when I copied it and it was 19:48, which is after the changes in question. That means a merge must have taken place by that point, and the wal couldn\'t have had something from the missing period, as those changes were already merged.
' %}

The folders I marked with a dagger † contain folders named with base64 hashes of length 44. The hashes stay the same, i.e. a particular document has the same hash in the three different folders. This is the document id (which I will henceforth refer to as `[docid]`). It's also in the url when we edit a document in a web browser: docs.google.com/document/d/[docid]/edit.

Structure of each document subfolder under each of the three daggers:

(1) __[guid]/fileStore/documents__
```
 [docid]/
 │
 └── documents/
     └── [docid]/
         └── image/
```
has images embedded in each document. Documents that don't have any don't have a folder here.

(2) __[guid]/localStore/documents__
```
 [docid]/
 │
 └── [docid].db
```
The save file for each document, it seems!

(3) __drivekit/users/[guid]/thumbnails__
```
 [docid]/
 │
 └── [docid]-[epoch]
```
where [epoch] is a unix epoch timestamp like 1736469298297 (2025-01-10 00:34 UTC in this case). An image preview of each document. Some documents don't have one and the folder is just empty.

drivekit/users/[guid]/files is empty in my case. Maybe it would have contained a similar structure if I had documents that contained something.

I also looked at all the template thumbnails for some reason (Documents/[guid]/fileStore/globalFiles/templates/thumbnail), there are 35 and they're all in French (though the app itself is in English) dated 2025-07-01 10:36.

All dead ends apart from [guid]/localStore/documents (2). That's the only thing resembling a save, so I have to assume that's how the offline documents are handled also.

Save database tables:
- `VersionedDatabaseVersion` : 70 rows of `current_version` 9
- `blob_metadata` : no rows
- `blob_metadata_properties` : no rows
- `document_commands` : †
- `document_entities` : no rows
- `document_entity_properties` : no rows
- `document_properties` : things like id, title, mimeType. values are byte-arrays of various lengths
- `documents` : just the one ([docid]). `prefer_warm_start` 0, `is_database_valid` 2
- `pending_queue_command_bundles` : no rows
- `pending_queue_commands` : no rows
- `pending_queue_properties` : kind of like document properties things like docId, revision, values are byte-arrays of various lengths
- `pending_queues` : one row with [docid]
- `sqlite_sequence` : 2 rows. name `documents` seq 1, name `pending_queues` seq 1

(you can see the contents of each table with the query `SELECT * FROM [tablename]`)

All dead ends apart from `document_commands`. Each row of it contains:
- `document_id` : always [docid]
- `part_id` : seemingly always 0
- `revision` : increments for each row
- `chunk_index` : usually 0
- `timestamp` : unix epoch timestamp of command, or 0
- `serialized_commands` : the actual data

In the save db for my document there are 934 rows, and the first three have timestamp 0, revision 30327, and chunk index 0 1 2. The `serialized_commands` for each of them is a string with the following general structure:
```json
[{"ty":"is","ibi":1,"s":"[..snip..]"},
 {"ty":"as","st":"document","si":0,"ei":0,"sm":{"ds_df":{"df_dm":1},"ds_lhs":1,"ds_ulhfl":false}},
 {"ty":"ae","et":"list", [..snip..]},
 {"ty":"as","st":"headings","si":0,"ei":0,"sm":{[..snip..]}},
 {"ty":"as","st":"language","si":0,"ei":0,"sm":{"lgs_l":"fr"}},
 {"ty":"as","st":"paragraph","si":42,"ei":42,"sm":{[..snip..]}},
 {"ty":"null"},
 [..many more ty as, st paragraph / list / text..]
 {"ty":"as","st":"text","si":2315,"ei":2374,"sm":{"ts_fs":28,"ts_fs_i":true}},
 {"ty":"as","st":"text","si":2375,"ei":2450,"sm":{"ts_fs":28,"ts_fs_i":false}}]
```
where on the first line, the first snip (the `"s"`) contains a portion of the text of the document. It seems to me that the aforementioned first three rows contain "the base" of the document (the entire document up to a certain point in time, the start of the current editing session presumably), then the commands that follow in subsequent rows represent edits made on top of this base. How many of these rows there will be at the start depends on the length of the document.

`"ty"` seems to be the type of modification, where `"ds"` is deletion:
```json
[{"ty":"ds","si":5752,"ei":6599}]
```
and `"mlti"` is I guess a command that contains several commands, `"is"` is insertion, and `"as"` is style or property.
```json
[{"ty":"mlti", "mts":[
  {"ty":"is","ibi":5752,"s":"\\n"},
  {"ty":"as","st":"paragraph","si":5752,"ei":5752,"sm":{[..snip..]}}]},
 {"ty":"is","ibi":5753,"s":"-"},
 {"ty":"ds","si":5753,"ei":5753},
 {"ty":"is","ibi":5753,"s":"—"},
 {"ty":"is","ibi":5754,"s":"-"},
 {"ty":"mlti","mts":[
   {"ty":"is","ibi":5755,"s":"\\n"},
   {"ty":"as","st":"paragraph","si":5755,"ei":5755,"sm":{[..snip..]}}]},
 {"ty":"mlti","mts":[
   {"ty":"is","ibi":5756,"s":"\\n"},
   {"ty":"as","st":"paragraph","si":5756,"ei":5756,"sm":{[..snip..]}}]},
 {"ty":"is","ibi":5757,"s":"1"},
 {"ty":"is","ibi":5758,"s":"1"},
 {"ty":"is","ibi":5759,"s":":"},
 {"ty":"is","ibi":5760,"s":"3"},
 {"ty":"is","ibi":5761,"s":"1"},
 {"ty":"is","ibi":5762,"s":" "},
 {"ty":"is","ibi":5763,"s":"b"},
 {"ty":"is","ibi":5764,"s":"i"},
 {"ty":"is","ibi":5765,"s":"e"},
 {"ty":"is","ibi":5766,"s":"n"},
 {"ty":"is","ibi":5767,"s":" "},
 {"ty":"is","ibi":5768,"s":"s"},
 {"ty":"is","ibi":5769,"s":"u"},
 {"ty":"is","ibi":5770,"s":"r"},
 {"ty":"is","ibi":5771,"s":" "},
 {"ty":"is","ibi":5772,"s":"q"},
 {"ty":"is","ibi":5773,"s":"u"},
 {"ty":"is","ibi":5774,"s":"e"},
 {"ty":"is","ibi":5775,"s":" "},
 {"ty":"is","ibi":5776,"s":"c"},
 {"ty":"ds","si":5776,"ei":5776},
 {"ty":"is","ibi":5776,"s":"ç"},
 {"ty":"is","ibi":5777,"s":"a"},
 {"ty":"is","ibi":5778,"s":" "},
 {"ty":"is","ibi":5779,"s":"a"},
 {"ty":"is","ibi":5780,"s":"r"},
 {"ty":"is","ibi":5781,"s":"r"},
 {"ty":"is","ibi":5782,"s":"i"},
 {"ty":"is","ibi":5783,"s":"b"},
 {"ty":"ds","si":5783,"ei":5783},
 {"ty":"is","ibi":5783,"s":"v"},
 {"ty":"is","ibi":5784,"s":"e"},
 [..snip..]]
```
btw if using flow keyboard or autocomplete, you can have more than one character in the `"s"`:
```json
{"ty":"is","ibi":12602,"s":"d’arrêter"}
```

{% include note.html content='
> [!NOTE]
> Killing the app from the app switcher and relaunching it gets rid of all the commands other than the timestamp 0 "base" ones.
' %}

Extracting the document:
```sh
# scp root@[ip]:[source] [destination]
scp root@10.100.102.8:/var/mobile/Containers/Data/Application/1EEC8B78-8AC3-4680-9D64-184F6BA1E697/Documents/[guid]/localStore/documents/[docid]/[docid].db /media/pnotes/py/data/2026-03-01-verdicts2-avant.db
```
where `10.100.102.8` should be the local network ip assigned to the device (on iOS 13.5.1 you can check on Settings, Wi-Fi, blue `i` icon to the right of the network, IPV4 ADDRESS, IP Address). The device must have OpenSSH set up (here it's a jailbroken device with OpenSSH installed via Cydia). and the hash for the folder name, google user id, and document id will differ.

Print rows via Python:
```python
import sqlite3
db = 'data/2026-03-01-verdicts2-avant.db'
con = sqlite3.connect(db)
cur = con.cursor()
for row in cur.execute("SELECT * FROM document_commands"):
    print(row)
con.close()
```

Naïve script to extract just the data semi-readably, ignoring deletions so the output has all the typos included:
```python
import sqlite3
db = 'data/2026-03-01-verdicts2-avant.db'
con = sqlite3.connect(db)
cur = con.cursor()
output = []
for row in cur.execute("SELECT * FROM document_commands"):
    cmds = row[5]
    if type(cmds) is str:
        cmds = eval(cmds.replace('false', 'False').replace('true', 'True').replace('null', 'None'))
    for cmd in cmds:
        s = cmd.get('s')
        if s:
            output.append(s)
print(''.join(output))
con.close()
```
instead of `eval` with replacements, I should have used `json.loads`.

but I stopped there because I noticed that the database did not have the data I was looking for. It does have data not present in the current form of the document, like things I had deleted, but specifically the bit that disappeared is not present. The last modifications are from right before, 18:57, and my modifications that disappeared will have been around 19:25, and that's gone. The last three rows:
```python
('[docid]', '0', 51256, 0, 1772299444133, '[{"ty":"ds","si":6049,"ei":12612}]')
('[docid]', '0', 51257, 0, 1772300813977, '[{"ty":"mlti","mts":[{"ty":"null"},{"ty":"null"}]},{"ty":"mlti","mts":[{"ty":"null"},{"ty":"null"}]},{"ty":"null"},{"ty":"null"},{"ty":"null"},{"ty":"null"},{"ty":"null"}]')
('[docid]', '0', 51258, 0, 1772300858336, '[{"ty":"null"}, [..ty null 872 times..]]')
```
I don't know if it's normal to have a tonne of nulls at the end, but whatever the case may be, the timestamps here are not correct for when the edits will have taken place.

I had high hopes for this database because it had things from even before what I needed that are no longer in the current document, and all the edits you make seem to be registered, yet exactly the bit I needed is not there. I am not sure what happened there. Just another dead end. and I think this time for good.

### More information on incident
not sure what I did to trigger it. It did not happen immediately when I returned. I had written a list of things to pack and when I returned I was packing while consulting the list. Some minutes into doing this, the list disappeared as I was looking at it; the document refreshed itself to match with the online version from before the local offline edits began. Or maybe I wasn't looking at it, I don't know if I trust my memory because I was also under stress, but suffice to say I was packing following a list and then it was gone.

I also tried to undo, which did nothing.

Another potential point of friction is when you modify a document you go into edit mode by pressing the pen button, then you can continue to edit in this mode and leave it like that, forgetting to press the tick to finish the edit. and maybe I had not pressed the tick. Testing now making edits, I see that whether or not I press the tick the db does not change, and whether or not I press the tick the db-wal does change. When I press the < button to "close the document" / go back to the screen with all the documents, it merges the db-wal to the db file.

Another possibility is modifying the document on the browser before leaving again, and the app not syncing yet, as it's sometimes delayed and you sometimes have to kill and reopen it for it to sync changes from another device and I can't remember if I did this, then that while I was away I made modifications on a superseded revision.

I'm on version 1.2022.20202 on iOS 13.5.1, admittedly it is a few years old and maybe recent versions will be more robust, but I don't think that I will be able to trust Google Docs again with offline editing.

## iOS Messages extraction
While we're at it, let's have a go extracting data from an iOS Messages conversation seeing that it's quite similar.

It's located in /var/mobile/Library/SMS/sms.db. This time I am making sure to look at the db-wal and db-shm files as well.

- 2026-03-06 03:11 db 17.4 MB
- 2026-03-07 12:55 db-wal 688 KB
- 2025-11-16 03:04 db-shm 66 KB

The conversation I want is from 02-28, so the db file alone should do the job.
```sh
# scp root:[ip]:[source] [destination]
scp root@192.168.1.11:/var/mobile/Library/SMS/sms.db /media/pnotes/py/data/2026-03-07-sms.db
```
For observant ones, my device internal network ip is different now because I am not in the same place. If it's wrong it just times out.

Python script to see the names of all the tables:
```python
import sqlite3
smsdb = 'data/2026-03-07-sms.db'
con = sqlite3.connect(smsdb)
cur = con.cursor()
for s in cur.execute("SELECT name FROM sqlite_master WHERE type='table'"):
    print(s)
con.close()
```
Output:
```python
('_SqliteDatabaseProperties',)
('deleted_messages',)
('sqlite_sequence',)
('chat_handle_join',)
('sync_deleted_messages',)
('message_processing_task',)
('handle',)
('sync_deleted_chats',)
('message_attachment_join',)
('sync_deleted_attachments',)
('kvtable',)
('chat_message_join',)
('message',)
('chat',)
('attachment',)
('sqlite_stat1',)
```

To get data of a particular column we could execute `SELECT * FROM chat` but the result is tuples with just the data, no column names. Can set `con.row_factory = sqlite3.Row` to instead get these `Row` objects which we can convert to a dictionary.
```python
def exe(db, cmd):
    con = sqlite3.connect(db)
    con.row_factory = sqlite3.Row
    cur = con.cursor()
    for s in cur.execute(cmd):
        print(dict(s))
    con.close()

exe(smsdb, "SELECT * FROM chat")
```
Excerpt output for one of the rows:
```python
{'ROWID': 103, 'guid': 'SMS;-;[..snip..]', 'style': 45, 'state': 3, 'account_id': '[..snip..]', 'properties': '[..snip..]', 'chat_identifier': '[..snip..]', 'service_name': 'SMS', 'room_name': None, 'account_login': 'E:', 'is_archived': 0, 'last_addressed_handle': '[..snip..]', 'display_name': '', 'group_id': '[..snip..]', 'is_filtered': 0, 'successful_query': 0, 'engram_id': None, 'server_change_token': '', 'ck_sync_state': 0, 'original_group_id': '[..snip..]', 'last_read_message_timestamp': 0, 'sr_server_change_token': '', 'sr_ck_sync_state': 0, 'cloudkit_record_id': '', 'sr_cloudkit_record_id': '', 'last_addressed_sim_id': '[..snip..]', 'is_blackholed': 0}
```

I can see that the conversation I want is row id 2.

but you can just do directly
```python
exe(smsdb, "SELECT * FROM message")
```
as the rows you get are ordered chronologically, with the last one being the most recent message.

Message structure:
```python
{'ROWID': 10247, 'guid': '[..snip..]', 'text': '[..snip..]', 'replace': 0, 'service_center': None, 'handle_id': 2, 'subject': None, 'country': None, 'attributedBody': '[..snip..]', 'version': 10, 'type': 0, 'service': 'iMessage', 'account': 'p:[..snip..]', 'account_guid': '[..snip..]', 'error': 0, 'date': 794529672156682752, 'date_read': 0, 'date_delivered': 0, 'is_delivered': 1, 'is_finished': 1, 'is_emote': 0, 'is_from_me': 0, 'is_empty': 0, 'is_delayed': 0, 'is_auto_reply': 0, 'is_prepared': 0, 'is_read': 0, 'is_system_message': 0, 'is_sent': 0, 'has_dd_results': 0, 'is_service_message': 0, 'is_forward': 0, 'was_downgraded': 0, 'is_archive': 0, 'cache_has_attachments': 0, 'cache_roomnames': None, 'was_data_detected': 1, 'was_deduplicated': 0, 'is_audio_message': 0, 'is_played': 0, 'date_played': 0, 'item_type': 0, 'other_handle': 0, 'group_title': None, 'group_action_type': 0, 'share_status': 0, 'share_direction': 0, 'is_expirable': 0, 'expire_state': 0, 'message_action_type': 0, 'message_source': 0, 'associated_message_guid': None, 'associated_message_type': 0, 'balloon_bundle_id': None, 'payload_data': None, 'expressive_send_style_id': None, 'associated_message_range_location': 0, 'associated_message_range_length': 0, 'time_expressive_send_played': 0, 'message_summary_info': '[..snip..]', 'ck_sync_state': 0, 'ck_record_id': None, 'ck_record_change_tag': None, 'destination_caller_id': '[..snip..]', 'sr_ck_sync_state': 0, 'sr_ck_record_id': None, 'sr_ck_record_change_tag': None, 'is_corrupt': 0, 'reply_to_guid': '[..snip..]', 'sort_id': 0, 'is_spam': 0}
```

I don't know if it's by coincidence that it corresponds with the row id in chat, but it seems like all of the messages from the conversation I want have `handle_id: 2`
```python
exe(smsdb, "SELECT * FROM message WHERE handle_id=2")
```

Final script I used:
```python
import time
import sqlite3

smsdb = 'data/2026-03-07-sms.db'

# same as before, I just changed print to yield
def exe(db, cmd):
    con = sqlite3.connect(db)
    con.row_factory = sqlite3.Row
    cur = con.cursor()
    for s in cur.execute(cmd):
        yield dict(s)
    con.close()

def appletime_to_str(t):
    # John Galbraith https://stackoverflow.com/a/36921288/18396947
    return time.strftime('%F %T', time.localtime(t/1000000000 + 978307200))

for r in exe(smsdb, "SELECT * FROM message WHERE handle_id=2"):
    # formatting the output
    print('-', appletime_to_str(r.get('date')),
          r.get('is_from_me') and 'M:' or 'E:',
          '«', r.get('text', '').replace("\n", "⏎"), '»')
```
I put a `-` at the start so that it's already preformatted for a markdown list, and replaced newlines with the symbol ⏎.

- 2026-03-05 20:23:00 M: « no »
- 2026-03-05 20:57:18 E: « Ok thanks »
- 2026-03-05 22:46:10 M: « it’s actually fine the rug on second thoughts »
- 2026-03-05 22:47:58 E: « Thanks!⏎Now i put it closer to the couch »
- 2026-03-05 22:48:43 M: « i waited but i heard detonations even though there wasn’t a siren »
- 2026-03-05 22:48:48 E: « ￼ »
- 2026-03-05 22:49:14 E: « I also heard. »
- 2026-03-05 22:49:56 M: « that’s nice. cosy »

This symbol: `￼` (U+FFFC, Object Replacement Character) seems to be what shows up in the text when there is an attachment. In this case my interlocutor sent me an image of a rug. It's the rug that I found cosy, not the detonations.

I'm surprised that it's this easy, I remember years ago looking for how to extract messages and only finding paid software, nothing open source. Looks like since 2022 there is [imessage-exporter by Christopher Sardegna](https://github.com/ReagentX/imessage-exporter).

If you don't have filesystem access, the `sms.db` [also exists in backups](https://www.reddit.com/r/iOSProgramming/comments/1bp458a/comment/llcpfc2/).

## Conclusion of sorts
It's not my first time doing data extraction from iOS, but it is my first time finding useful data in db files on iOS. It seems like they are used extensively on this platform so it's important to have got some experience working with them, and to have learned not to overlook them in future. Formerly when encountering db files I used the builtin viewer and never found anything useful. They are a bit obtuse compared to something like a json file. It can take a while going through the tables. It's annoying too that sometimes opening an sqlite database to read modifies it. I now always make a copy first, but how to properly deal with wal and shm I still don't know.

{% include fin.html %}
