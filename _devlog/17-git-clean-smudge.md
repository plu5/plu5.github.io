---
layout: post
title: 17 — Encryption with git clean/smudge filters
date: 2026-05-15 05:49
modified_date: 2026-06-28 05:19
categories: git cryptography
lang: en
redirect_from: /devlog/17
---

## Git attributes
Smudge and clean filters are set in [`.gitattributes`](https://git-scm.com/book/en/v2/Customizing-Git-Git-Attributes) ([fr](https://git-scm.com/book/fr/v2/Personnalisation-de-Git-Attributs-Git)) (or `.git/info/attributes`), which I currently know only as "the thing controlling what happens to line endings". I personally prefer for line endings to be kept as they are, so I often have a `.gitattributes` with `* -text` (disable "text" attribute for all files, which means it doesn't try to do line endings normalisation). Git attributes can also be used to control what diff does for different file types, avoid diffing certain files altogether (mark as binary), [exclude files from export](https://coder-joey.github.io/Utilising-Git-Attributes/), substitute format strings in export (`export-subst`, I imagine this is rarely used), ignore or prioritise certains files in a merge, or transform file contents.

> When adding content from the working directory to the staging area with `git add`, files that match the `.gitattributes` record will go through the clean filter. When pulling content back into the working directory with `git pull`, those same files will go through the smudge filter.  
—[*Tomer Figenblat, Red Hat Developer, 2022*](https://developers.redhat.com/articles/2022/02/02/protect-secrets-git-cleansmudge-filter#)

In the case of using it for encryption or hiding secrets, it may be the opposite terms from what you would expect: "clean" = obfuscate, and "smudge" = deobfuscate.

The [previously-linked git book](https://git-scm.com/book/en/v2/Customizing-Git-Git-Attributes) gives the following example smudge/clean filters:
```text
*.c filter=indent
```
```sh
$ git config --global filter.indent.clean indent
$ git config --global filter.indent.smudge cat
```
In order to automatically indent C code that gets added to the repository. The clean calls `indent` on the code (some hypothetical CLI program in PATH). The smudge just calls `cat`, so does no transformation and returns the contents as is.

## Nonexistent filter
What happens if you give a filter and nothing by that name is set in git config? Will it error when adding files, or just do nothing?

Attempt:
```sh
mkdir git-filters-test
cd git-filters-test
git init
echo "hi" > hi.txt
echo "*.txt filter=crypt" > .gitattributes
git add .gitattributes
git add hi.txt
```
No errors :-(

## CLI tool for encryption/decryption
I thought there would be a CLI tool included with Linux that everyone uses, but the situation is more messy.

- GPG? The symmetric encryption uses AES with its own mode. Is there authentication in that mode? No. And [you can't give your own key](https://crypto.stackexchange.com/questions/101363/gnupg-provide-encryption-key-for-aes-rather-than-deriving-it-from-passphrase), only a password that it will derive a key from ([using S2K](https://crypto.stackexchange.com/questions/48952/what-is-the-key-derivation-function-used-in-gnupg-and-how-long-may-its-input-be), no ability to use PBKDF2).
- OpenSSL? `enc` only supports old algorithms/modes, for AES for example it only has ECB (REALLY DON'T!) and CBC. `cms` supports GCM, but we can't just give it a key, it requires "recipient(s) certificate(s)"
- age? Doesn't let the user decide anything, uses its own thing for encryption, name puts me off ("Actually Good Encryption", talk about hubris. I guess it's inspired by PGP "Pretty Good Privacy" -- but I feel like that's quite humble in comparison)

Searched in the package manager of Arch Linux aes, jwe, gcm, sodium, nacl, without finding CLI tools.

I think [OpenSSL CMS](https://docs.openssl.org/master/man1/openssl-cms/) is the most promising here. There's more complexity because it requires a certificate, but if we can use AES-GCM with it, understand the format it uses to store the data (which part is the actual ciphertext, what is the IV), and ideally have a way to get the key used to encrypt in order to be able to decipher with any other tool, then it should be fine.

As for why openssl enc doesn't support GCM:

> This command does not support authenticated encryption modes like CCM and GCM, and will not support such modes in the future. This is due to having to begin streaming output (e.g., to standard output when -out is not used) before the authentication tag could be validated. When this command is used in a pipeline, the receiving end will not be able to roll back upon authentication failure. The AEAD modes currently in common use also suffer from catastrophic failure of confidentiality and/or integrity upon reuse of key/iv/nonce, and since openssl enc places the entire burden of key/iv/nonce management upon the user, the risk of exposing AEAD modes is too great to allow. These key/iv/nonce management issues also affect other modes currently exposed in this command, but the failure modes are less extreme in these cases, and the functionality cannot be removed with a stable release branch. For bulk encryption of data, whether using authenticated encryption modes or other modes, [openssl-cms(1)](https://docs.openssl.org/master/man1/openssl-cms/) is recommended, as it provides a standard data format and performs the needed key/iv/nonce management.  
—[*man openssl-enc: Supported Ciphers*](https://docs.openssl.org/master/man1/openssl-enc/#supported-ciphers)

## OpenSSL CMS certificates: Generating, encrypting, decrypting
For generating a certificate, [openssl req](https://docs.openssl.org/master/man1/openssl-req/) is used.

```sh
openssl req -newkey rsa:2048 -keyout key.pem -out req.pem
```
It prompts for a 4-to-1024-character password, and optionally: 2-letter country name, state or province name, locality name, organisation name, organisational unit name, common name, email address, challenge password, optional company name.

`key.pem`:
```text
-----BEGIN ENCRYPTED PRIVATE KEY-------
[..long base64 hash..]
-----END ENCRYPTED PRIVATE KEY-----
```

`req.pem`:
```text
-----BEGIN CERTIFICATE REQUEST-----
[..long base64 hash..]
-----END CERTIFICATE REQUEST-----
```

Trying to use it:
```sh
openssl cms -encrypt -aes-256-gcm -in hi.txt -out enc.bin req.pem
```

> Could not find recipient certificate file from req.pem

needed to also pass `-x509` when generating the certificate

```sh
openssl req -x509 -newkey rsa:2048 -keyout key.pem -out req.pem
openssl cms -encrypt -aes-256-gcm -in hi.txt -out enc.bin req.pem
```

`enc.bin` structure (including the empty lines):
```text
MIME-Version: 1.0
Content-Disposition: attachment; filename="smime.p7m"
Content-Type: application/pkcs7-mime; smime-type=authEnveloped-data; name="smime.p7m"
Content-Transfer-Encoding: base64

[..long base64 hash..]

```

{% include note.html content='
> [!NOTE]
> [S/MIME](https://en.wikipedia.org/wiki/S/MIME)
' %}

Second attempt at decrypting:
```sh
openssl cms -decrypt -in enc.bin -recip req.pem -inkey key.pem
```
Have to type the password we gave when creating the private key. Then it outputs the decrypted text to stdout.

## OpenSSL CMS certificates: Decrypt with only the key
What if we lose the certificate and only have the private key? You can generate a certificate from the private key (see [examples section on the man page](https://docs.openssl.org/master/man1/openssl-req/#examples)) (remember the `-x509`).
```sh
rm req.pem
openssl req -x509 -new -key key.pem -out req2.pem
openssl cms -decrypt -in enc.bin -recip req2.pem -inkey key.pem
```

> Error decrypting CMS using private key

Even if I give the same details when creating the certificate.

We can examine the CMS data structure with `-cmsout -print`:
```sh
$ openssl cms -cmsout -print -in enc.bin
CMS_ContentInfo: 
  contentType: id-smime-ct-authEnvelopedData (1.2.840.113549.1.9.16.1.23)
  d.authEnvelopedData: 
    version: 0
    originatorInfo: <ABSENT>
    recipientInfos:
      d.ktri: 
        version: 0
        d.issuerAndSerialNumber: 
          issuer:           C=AU, ST=Some-State, O=Internet Widgits Pty Ltd
          serialNumber: 0x1A97D4B549158161C4B70310EA11CADB13ADD0B8
        keyEncryptionAlgorithm: 
          algorithm: rsaEncryption (1.2.840.113549.1.1.1)
          parameter: NULL
        encryptedKey: 
          0000 - 25 36 6c be f7 31 ad a1-5a cd 58 ff 38 01 88   %6l..1..Z.X.8..
          000f - 93 56 72 11 0c e4 fe 2f-3e 52 7c 34 21 52 fc   .Vr..../>R|4!R.
          001e - ba 7a 27 94 9c 93 f6 12-45 60 0f d0 93 a7 47   .z'.....E`....G
          002d - f7 f0 89 85 21 72 15 cd-12 8a d8 3e db 6e 63   ....!r.....>.nc
          003c - 70 d1 9b f1 07 84 77 3a-dc 92 48 3b c8 79 34   p.....w:..H;.y4
          004b - 52 16 93 e2 94 ce 10 70-75 2f 83 30 ff 5b 86   R......pu/.0.[.
          005a - d0 7d 94 19 54 02 fb e9-66 bf 96 6f 8f 08 22   .}..T...f..o.."
          0069 - 6b 24 75 e3 eb 20 f1 f6-87 93 6f 17 74 f9 1d   k$u.. ....o.t..
          0078 - 04 f5 06 6f 25 21 77 1f-bb 90 e6 db 47 41 d6   ...o%!w.....GA.
          0087 - be 5b 8c 1e f7 2c 34 ee-e3 9f 20 f6 19 b3 38   .[...,4... ...8
          0096 - 56 28 55 29 9a 0b e9 2f-ea dc 2d 89 3f a7 c4   V(U).../..-.?..
          00a5 - b7 de e8 23 95 96 4c bd-3a ea ba d3 a1 e8 ad   ...#..L.:......
          00b4 - 4c 59 cd 3b ef c5 e5 d3-13 0a 56 75 8e 38 6b   LY.;......Vu.8k
          00c3 - 1a 2a 5d 9a af 85 58 6b-3a 4c 37 3e 21 12 4b   .*]...Xk:L7>!.K
          00d2 - bc 37 3e bc 37 43 89 95-00 78 56 59 7f 9f b3   .7>.7C...xVY...
          00e1 - b3 72 4c 73 a9 31 53 61-f4 e2 0e 4b 9f a5 94   .rLs.1Sa...K...
          00f0 - 9b d5 d0 20 42 ff 45 74-4d 05 3a 64 f5 43 d8   ... B.EtM.:d.C.
          00ff - c8                                             .
    authEncryptedContentInfo: 
      contentType: pkcs7-data (1.2.840.113549.1.7.1)
      contentEncryptionAlgorithm: 
        algorithm: aes-256-gcm (2.16.840.1.101.3.4.1.46)
        parameter: SEQUENCE:
    0:d=0  hl=2 l=  17 cons: SEQUENCE          
    2:d=1  hl=2 l=  12 prim:  OCTET STRING      [HEX DUMP]:25319A8AA4B76B14D5C7ADCB
   16:d=1  hl=2 l=   1 prim:  INTEGER           :10
      encryptedContent: 
        0000 - e0 f5 9b 09                                    ....
    authAttrs:
      <ABSENT>
    mac: 
      0000 - 0c d1 b9 e4 79 f7 72 57-19 30 02 9e f7 59 6f b3   ....y.rW.0...Yo.
    unauthAttrs:
      <ABSENT>
```
(our plaintext is just "hi", so encryptedContent is very small)

{% include note.html content='
> [!NOTE]
> What follows is a lot of mistakes and nested confusion, you have been warned. You may wish to skip to the end of the section.
' %}

`rsaEncryption (1.2.840.113549.1.1.1)` is used to encrypt the key. 1.2.840.113549.1.1.1 [is the OID](https://oid-base.com/get/1.2.840.113549.1.1.1) of `RSAES-PKCS1-v1_5`.

openssl cms actually has a keyopt option that we can pass OAEP to:

> -keyopt name:parameter  
For signing and encryption this option can be used multiple times to set customised parameters for the preceding key or certificate. It can currently be used to set RSA-PSS for signing, RSA-OAEP for encryption or to modify default parameters for ECDH.  
—[*openssl cms man page: Keys and password options*](https://docs.openssl.org/master/man1/openssl-cms/#keys-and-password-options)

In the examples it's passed with `-keyopt rsa_padding_mode:oaep`.

Let's try this again then.
```sh
rm enc.bin
openssl cms -encrypt -aes-256-gcm -keyopt rsa_padding_mode:oaep -in hi.txt -out enc.bin req2.pem
```

> No key specified

Is this maybe the wrong order? In the example they give keyopt after the pem.
```sh
openssl cms -encrypt -aes-256-gcm -in hi.txt -out enc.bin req2.pem -keyopt rsa_padding_mode:oaep
```

> Could not open file or uri for loading recipient certificate file from -keyopt: No such file or directory

They also put `-recip` before the pem. Try that?
```sh
openssl cms -encrypt -aes-256-gcm -in hi.txt -out enc.bin -recip req2.pem -keyopt rsa_padding_mode:oaep
```
Yes, that worked.

`openssl cms -cmsout -print -in enc.bin` now has `algorithm: rsaesOaep (1.2.840.113549.1.1.7)` in keyEncryptionAlgorithm.

The benefit of this is WebCrypto has RSA-OAEP, so we can decrypt the key easily using a browser.

{% include note.html content='
> [!NOTE]
> Here it seems like I got confused. I talk above about decrypting the key, then below I try to import it as is, which I obviously can\'t, as it\'s encrypted.
' %}

I tried to import key.pem by removing the newlines in it and any `=` padding characters from the end, and converting the `+` and `/` to `-` and `_`, but it's sadly not as simple as that.
```javascript
await window.crypto.subtle.importKey("jwk", {k: "[..long base64 hash..]", kty: "RSA"}, "RSA-OAEP", true, ["encrypt", "decrypt"])
```

> Uncaught (in promise) DOMException: Data provided to an operation does not meet requirements

Testing exporting an RSA key generated by WebCrypto, I see that the hash is split into several parameters.
```javascript
keyPair = await window.crypto.subtle.generateKey({name: "RSA-OAEP", modulusLength: 4096, publicExponent: new Uint8Array([1, 0, 1]), hash: "SHA-256"}, true, ["encrypt", "decrypt"]);
await window.crypto.subtle.exportKey("jwk", keyPair.privateKey);
```
`d`, `dp`, `dq`, `e`, `n`, `p`, `q`, `qi`.

It might be simpler to import as raw instead of jwk. For that we're going to need to convert our base64 key to an array of numbers, 1 byte each.
```javascript
const base64ToUint8Array = (base64) => Uint8Array.from(
  atob(base64), c => c.charCodeAt(0));
```
(note that this will only work with the non-url-safe variant of base64, i.e. the one with `+` and `/`, not `-` and `_`)
```javascript
k = base64ToUint8Array("[..long base64 string..]")
await window.crypto.subtle.importKey("raw", k, "RSA-OAEP", true, ["encrypt", "decrypt"])
```

> Uncaught (in promise) DOMException: Data provided to an operation does not meet requirements

For RSA we need to pass RsaHashedImportParams instead of the "RSA-OAEP" (cf [docs for importKey](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey)).
```javascript
await window.crypto.subtle.importKey("raw", k, {name: "RSA-OAEP", hash: "SHA-256"}, true, ["encrypt", "decrypt"])
```

> Uncaught (in promise) DOMException: An invalid or illegal string was specified

I think it's maybe that we can't import RSA with raw.

> Raw  
You can use this format to import or export AES or HMAC secret keys, or Elliptic Curve public keys (ECDSA or ECDH).  
—[*MDN: SubtleCrypto: importKey() method*](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey)

There's the PKCS #8 format for it. And the docs even say how to convert a PEM into a format we can give importKey.

> - base64-decode the part between header and footer, using Window.atob().
- convert the resulting string into an ArrayBuffer.

So what we tried before, just with `"pkcs8"` instead of `"raw"`?

```javascript
await window.crypto.subtle.importKey("pkcs8", k, "RSA-OAEP", true, ["encrypt", "decrypt"])
```
It's still unhappy.

> Uncaught (in promise) DOMException: Data provided to an operation does not meet requirements

The [docs](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey) have example functions to do the conversion, see `function importPrivateKey(pem)`. I don't want to paste here because it's very long. You can paste the entire PEM string in between grave characters (to avoid having to escape the newlines).
```javascript
importPrivateKey(`[..long pem string..]`)
```
But WebCrypto is not happy about that one either.

> Uncaught DOMException: String contains an invalid character

They use:
```javascript
  const pemHeader = "-----BEGIN PRIVATE KEY-----";
  const pemFooter = "-----END PRIVATE KEY-----";
```
Whereas for our PEM it's:
```text
-----BEGIN ENCRYPTED PRIVATE KEY-----
-----END ENCRYPTED PRIVATE KEY-----
```
Hang on, encrypted? Does it mean it's not really the key?

I'm going to try anyway to modify their code to just take the base64 string without header or footer. And also the algorithm because I see in the example it's `"RSA-PSS"`, so it wouldn't have worked anyway.
```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}

// Takes PEM string but without the header or footer
function importPrivateKey(pem) {
  // base64 decode the string to get the binary data
  const binaryDerString = window.atob(pem);
  // convert from a binary string to an ArrayBuffer
  const binaryDer = str2ab(binaryDerString);

  return window.crypto.subtle.importKey(
    "pkcs8", binaryDer,
    {name: "RSA-OAEP", hash: "SHA-256"},
    true, ["encrypt", "decrypt"],
  );
}
```

> Uncaught (in promise) DOMException: Data provided to an operation does not meet requirements

Maybe we need to also remove newlines? Though they don't do this.
```javascript
// Takes PEM string but without the header or footer
function importPrivateKey(pem) {
  // base64 decode the string to get the binary data
  const binaryDerString = window.atob(pem.replace(/\s+/g, ""));  // just added replace
  // convert from a binary string to an ArrayBuffer
  const binaryDer = str2ab(binaryDerString);

  return window.crypto.subtle.importKey(
    "pkcs8", binaryDer,
    {name: "RSA-OAEP", hash: "SHA-256"},
    true, ["encrypt", "decrypt"],
  );
}
```

> Uncaught (in promise) DOMException: Data provided to an operation does not meet requirements

I don't know what more to try.

The fact that it's "ENCRYPTED PRIVATE KEY" could be the problem.

Remember at the end of the last section when we decrypted, we also had to enter the password. I guess the password is used for key encryption. But the CMS data structure said that keyEncryptionAlgorithm is RSA, so how was it encrypted with a password?

How to decrypt the key?

> Pour retirer le mot de passe d'une clef RSA (générée avec genrsa ou keybot
ou fichier contenant `-----BEGIN ENCRYPTED PRIVATE KEY-----`)
et obtenir une clef privée au format PEM sans mot de passe, utilisez :  
`openssl rsa -in fichier-clef-avec-password.pkey -out fichier-sans-password.key`  
—[TBS Certificats FAQ](https://www.tbs-certificats.com/FAQ/fr/522.html)

```sh
openssl rsa -in key.pem -out keydecrypted.pem
```
Prompts for password. It's been weeks since I generated the key so I could not remember it, but I just intuitively typed "hello" and that was it.

Maybe decrypting now with OpenSSL will work?
```sh
openssl req -x509 -new -key keydecrypted.pem -out req3.pem
openssl cms -decrypt -in enc.bin -recip req3.pem -inkey keydecrypted.pem
```
No.

> Error decrypting CMS using private key

If we use that one then we're not prompted for password, so I guess it is intelligent and knows whether it works with an encrypted private key or a decrypted one based on the PEM header, it just won't work because it refuses to decrypt with a different certificate than what it was encrypted with.

But at least now importing to WebCrypto should work with the decrypted key, and we should be able to decrypt the text that way.

This again:
```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}
function importPrivateKey(pem) {
  const binaryDerString = window.atob(pem.replace(/\s+/g, ""));
  const binaryDer = str2ab(binaryDerString);
  return window.crypto.subtle.importKey(
    "pkcs8", binaryDer,
    {name: "RSA-OAEP", hash: "SHA-256"},
    true, ["encrypt", "decrypt"],
  );
}
```
Called like:
```javascript
importPrivateKey(`[long hash without header and footer]`)
```

> DOMException: An invalid or illegal string was specified

But hang on, why am I importing it as RSA? That was the key decryption algorithm, and we don't need to decrypt the key, we need to decrypt the content which is encrypted with AES256 GCM.

AES-GCM keys can be imported as base64 directly with jwk
```javascript
function importPrivateKey(pem) {
  const s = pem.replace(/\s+/g, '').replace(/=/g, '').replace(/\+/g, '-').replace(/\//g, '_');
  return window.crypto.subtle.importKey(
    "jwk", {k: s, kty: "oct"}, "AES-GCM", true,
    ["encrypt", "decrypt"])
}
```

> DOMException: Data provided to an operation does not meet requirements

The decrypted key without padding is 1623 base64 characters [1647-1649 in keys I generate now, not sure how/why I wrote 1623]. ~~Base64 is 6 bits per character. That would be 9738 bits,~~ [[not quite](https://stackoverflow.com/questions/31875408/a-single-character-in-base64string-holds-how-many-bytes-1-2-or-more). also don't confuse this length with the *modulus length* of the key, which is 2048 bits according to `openssl rsa -in key.pem -text -noout`] whereas AES256 takes a key of 256 bits. So the decrypted key is still an RSA key? I'm confused, because if the content is encrypted with AES-GCM, what is this RSA key encrypting?

Another key. The AES key. And then the RSA key it is encrypted with is encrypted with something else? With the password?

```sh
$ openssl asn1parse -in key.pem -i
    0:d=0  hl=4 l=1333 cons: SEQUENCE          
    4:d=1  hl=2 l=  95 cons:  SEQUENCE          
    6:d=2  hl=2 l=   9 prim:   OBJECT            :PBES2
   17:d=2  hl=2 l=  82 cons:   SEQUENCE          
   19:d=3  hl=2 l=  49 cons:    SEQUENCE          
   21:d=4  hl=2 l=   9 prim:     OBJECT            :PBKDF2
   32:d=4  hl=2 l=  36 cons:     SEQUENCE          
   34:d=5  hl=2 l=  16 prim:      OCTET STRING      [HEX DUMP]:34376A48184A3781FCBCBE6E2D46D80A
   52:d=5  hl=2 l=   2 prim:      INTEGER           :0800
   56:d=5  hl=2 l=  12 cons:      SEQUENCE          
   58:d=6  hl=2 l=   8 prim:       OBJECT            :hmacWithSHA256
   68:d=6  hl=2 l=   0 prim:       NULL              
   70:d=3  hl=2 l=  29 cons:    SEQUENCE          
   72:d=4  hl=2 l=   9 prim:     OBJECT            :aes-256-cbc
   83:d=4  hl=2 l=  16 prim:     OCTET STRING      [HEX DUMP]:69026CAEE7D7F3B6A21E30192BD2C880
  101:d=1  hl=4 l=1232 prim:  OCTET STRING      [HEX DUMP]:[..2464 hex characters..]
```
```sh
$ openssl asn1parse -in keydecrypted.pem -i
    0:d=0  hl=4 l=1213 cons: SEQUENCE          
    4:d=1  hl=2 l=   1 prim:  INTEGER           :00
    7:d=1  hl=2 l=  13 cons:  SEQUENCE          
    9:d=2  hl=2 l=   9 prim:   OBJECT            :rsaEncryption
   20:d=2  hl=2 l=   0 prim:   NULL              
   22:d=1  hl=4 l=1191 prim:  OCTET STRING      [HEX DUMP]:[..2382 hex characters..]
```

In the CMS structure from before, we had:
```sh
        encryptedKey: 
          0000 - 25 36 6c be f7 31 ad a1-5a cd 58 ff 38 01 88   %6l..1..Z.X.8..
          000f - 93 56 72 11 0c e4 fe 2f-3e 52 7c 34 21 52 fc   .Vr..../>R|4!R.
          001e - ba 7a 27 94 9c 93 f6 12-45 60 0f d0 93 a7 47   .z'.....E`....G
          002d - f7 f0 89 85 21 72 15 cd-12 8a d8 3e db 6e 63   ....!r.....>.nc
          003c - 70 d1 9b f1 07 84 77 3a-dc 92 48 3b c8 79 34   p.....w:..H;.y4
          004b - 52 16 93 e2 94 ce 10 70-75 2f 83 30 ff 5b 86   R......pu/.0.[.
          005a - d0 7d 94 19 54 02 fb e9-66 bf 96 6f 8f 08 22   .}..T...f..o.."
          0069 - 6b 24 75 e3 eb 20 f1 f6-87 93 6f 17 74 f9 1d   k$u.. ....o.t..
          0078 - 04 f5 06 6f 25 21 77 1f-bb 90 e6 db 47 41 d6   ...o%!w.....GA.
          0087 - be 5b 8c 1e f7 2c 34 ee-e3 9f 20 f6 19 b3 38   .[...,4... ...8
          0096 - 56 28 55 29 9a 0b e9 2f-ea dc 2d 89 3f a7 c4   V(U).../..-.?..
          00a5 - b7 de e8 23 95 96 4c bd-3a ea ba d3 a1 e8 ad   ...#..L.:......
          00b4 - 4c 59 cd 3b ef c5 e5 d3-13 0a 56 75 8e 38 6b   LY.;......Vu.8k
          00c3 - 1a 2a 5d 9a af 85 58 6b-3a 4c 37 3e 21 12 4b   .*]...Xk:L7>!.K
          00d2 - bc 37 3e bc 37 43 89 95-00 78 56 59 7f 9f b3   .7>.7C...xVY...
          00e1 - b3 72 4c 73 a9 31 53 61-f4 e2 0e 4b 9f a5 94   .rLs.1Sa...K...
          00f0 - 9b d5 d0 20 42 ff 45 74-4d 05 3a 64 f5 43 d8   ... B.EtM.:d.C.
          00ff - c8                                             .
```
That's 256 bits, so I guess that must be the encrypted AES key that is encrypted with the RSA key.

So back to trying to import the RSA key. But I did already try to import the decrypted key as RSA above and it complained about it being "invalid or illegal".

We're passing SHA-256 as the hash, but does 1.2.840.113549.1.1.7 use SHA-256? There's no mention of SHA, just "id-RSAES-OAEP".

[RsaHashedImportParams](https://developer.mozilla.org/en-US/docs/Web/API/RsaHashedImportParams) says that it supports SHA-256, SHA-384, SHA-512, or SHA-1.

```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}
function importPrivateKey(pem) {
  const binaryDerString = window.atob(pem.replace(/\s+/g, ""));
  const binaryDer = str2ab(binaryDerString);
  return window.crypto.subtle.importKey(
    "pkcs8", binaryDer,
    {name: "RSA-OAEP", hash: "SHA-1"},
    true, ["encrypt", "decrypt"],
  );
}
```

> DOMException: An invalid or illegal string was specified

Earlier we made OpenSSL use OAEP by specifying `-keyopt rsa_padding_mode:oaep` when generating the key. Can we also specify the SHA?

In [*openssl cms man page: Keys and password options*](https://docs.openssl.org/master/man1/openssl-cms/#keys-and-password-options) there are only examples for `-keyopt rsa_padding_mode:pss`, `-keyopt rsa_padding_mode:oaep`, and `-keyopt ecdh_kdf_md:sha256`.

I found this [SE question](https://stackoverflow.com/questions/17784022/how-to-encrypt-data-using-rsa-with-sha-256-as-hash-function-and-mgf1-as-mask-ge) with an answer by Carlos Saltos, 2018, who uses `-pkeyopt rsa_oaep_md:sha256`.

```sh
openssl cms -encrypt -aes-256-gcm -in hi.txt -out enc3.bin -recip req3.pem -keyopt rsa_padding_mode:oaep -keyopt rsa_oaep_md:sha256
```
But wait, where is the key? How is this going to affect the key if it's already-- ah because it's the AES key that we're concerned about, the RSA key that will be used to encrypt with we already have, and these options are for the encryption of the AES key, that is generated anew I guess each time we encrypt.

`openssl cms -cmsout -print -in enc3.bin` now has:
```sh
        keyEncryptionAlgorithm: 
          algorithm: rsaesOaep (1.2.840.113549.1.1.7)
          parameter: SEQUENCE:
    0:d=0  hl=2 l=  43 cons: SEQUENCE          
    2:d=1  hl=2 l=  13 cons:  cont [ 0 ]        
    4:d=2  hl=2 l=  11 cons:   SEQUENCE          
    6:d=3  hl=2 l=   9 prim:    OBJECT            :sha256
   17:d=1  hl=2 l=  26 cons:  cont [ 1 ]        
   19:d=2  hl=2 l=  24 cons:   SEQUENCE          
   21:d=3  hl=2 l=   9 prim:    OBJECT            :mgf1
   32:d=3  hl=2 l=  11 cons:    SEQUENCE          
   34:d=4  hl=2 l=   9 prim:     OBJECT            :sha256
```
What's mgf1? [Mask Generation Function](https://stackoverflow.com/questions/49483281/is-mgf1-padding-assumed-in-system-security-cryptography-rsa-encrypt-method-with), which is always MGF1 with RSA-OAEP.

But our problem is with importing the RSA key, which hasn't changed, it's the same regardless of the keyopts we specify because they don't affect the RSA key itself, just how it's used. So I don't know why WebCrypto doesn't accept the key.

Does the fact it says it's "illegal" rather than "doesn't meet requirements" mean it's the wrong length or format or a padding issue?

Let's go back to the example from the [docs](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey), even though they import RSA-PSS (which can't be used to encrypt/decrypt in WebCrypto, only signing).

```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}

function importPrivateKey(pem) {
  // base64 decode the string to get the binary data
  const binaryDerString = window.atob(pem);
  // convert from a binary string to an ArrayBuffer
  const binaryDer = str2ab(binaryDerString);

  return window.crypto.subtle.importKey(
    "pkcs8", binaryDer, {name: "RSA-OAEP", hash: "SHA-256"}, true, ["decrypt"],
  );
}
```
Removed the header and footer bits and changed to OAEP.

And yes, it works now! What was I doing wrong? The removing newlines?

The code from before with the replacing newlines removed:
```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}
function importPrivateKey(pem) {
  const binaryDerString = window.atob(pem);  // just this line
  const binaryDer = str2ab(binaryDerString);
  return window.crypto.subtle.importKey(
    "pkcs8", binaryDer,
    {name: "RSA-OAEP", hash: "SHA-256"},
    true, ["encrypt", "decrypt"],
  );
}
```

> DOMException: An invalid or illegal string was specified

Both if I pass SHA-1 or SHA-256.

Is it because I also have "encrypt"?

```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}
function importPrivateKey(pem) {
  const binaryDerString = window.atob(pem);
  const binaryDer = str2ab(binaryDerString);
  return window.crypto.subtle.importKey(
    "pkcs8", binaryDer, {name: "RSA-OAEP", hash: "SHA-256"}, true, ["decrypt"],
  );
}
```

That one works. The only difference is `["decrypt"]` instead of `["encrypt", "decrypt"]`. I am really confused.

If I only give encrypt, it's also "invalid or illegal".

[`encrypt` _does_ support RSA-OAEP](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/encrypt). But our key is a private key. Normally in RSA you encrypt with the public key and decrypt with the private key, so that could be why.

This was really painful. And I might have not even found out about it and got stuck for another million years, because in the example I went back to they used sign, so it's not like going back to that is what made me find this out, it's only by arbitrarily and out of laziness only replacing it with decrypt instead of both. Or maybe it's just my ignorance that made this happen, because I have spent a tonne of time with AES but virtually none with RSA; if in RSA it's impossible to encrypt with the private key then I should have known not to provide encrypt in the usages.

The next step is to decrypt the AES key with the RSA key we imported.

I will first try to write a function that will take this input as a string:
```sh
          0000 - 25 36 6c be f7 31 ad a1-5a cd 58 ff 38 01 88   %6l..1..Z.X.8..
          000f - 93 56 72 11 0c e4 fe 2f-3e 52 7c 34 21 52 fc   .Vr..../>R|4!R.
          001e - ba 7a 27 94 9c 93 f6 12-45 60 0f d0 93 a7 47   .z'.....E`....G
          002d - f7 f0 89 85 21 72 15 cd-12 8a d8 3e db 6e 63   ....!r.....>.nc
          003c - 70 d1 9b f1 07 84 77 3a-dc 92 48 3b c8 79 34   p.....w:..H;.y4
          004b - 52 16 93 e2 94 ce 10 70-75 2f 83 30 ff 5b 86   R......pu/.0.[.
          005a - d0 7d 94 19 54 02 fb e9-66 bf 96 6f 8f 08 22   .}..T...f..o.."
          0069 - 6b 24 75 e3 eb 20 f1 f6-87 93 6f 17 74 f9 1d   k$u.. ....o.t..
          0078 - 04 f5 06 6f 25 21 77 1f-bb 90 e6 db 47 41 d6   ...o%!w.....GA.
          0087 - be 5b 8c 1e f7 2c 34 ee-e3 9f 20 f6 19 b3 38   .[...,4... ...8
          0096 - 56 28 55 29 9a 0b e9 2f-ea dc 2d 89 3f a7 c4   V(U).../..-.?..
          00a5 - b7 de e8 23 95 96 4c bd-3a ea ba d3 a1 e8 ad   ...#..L.:......
          00b4 - 4c 59 cd 3b ef c5 e5 d3-13 0a 56 75 8e 38 6b   LY.;......Vu.8k
          00c3 - 1a 2a 5d 9a af 85 58 6b-3a 4c 37 3e 21 12 4b   .*]...Xk:L7>!.K
          00d2 - bc 37 3e bc 37 43 89 95-00 78 56 59 7f 9f b3   .7>.7C...xVY...
          00e1 - b3 72 4c 73 a9 31 53 61-f4 e2 0e 4b 9f a5 94   .rLs.1Sa...K...
          00f0 - 9b d5 d0 20 42 ff 45 74-4d 05 3a 64 f5 43 d8   ... B.EtM.:d.C.
          00ff - c8                                             .
```
and output an array buffer.

```javascript
function hexdump2ab(s) {
  const a = s.match(/(?<=(\s|-))[a-f0-9]{2}(?=(\s|-))/g);
  const res = new Uint8Array(Math.ceil(a.length));
  for (let i = 0; i < a.length; i++) {
    res[i] = Number.parseInt(a[i], 16);
  }
  return res;
}
```
For the algorithm to populate the Uint8Array, I consulted [this 2024 SE answer by undefined](https://stackoverflow.com/a/78634900/18396947). For the regex, [lookaheads and lookbehinds](https://javascript.info/regexp-lookahead-lookbehind).

In the encrypted key hex dump there is unfortunately a grave in the string (after the first E), so I had to remove that to be able to pass that in to hexdump2ab.

Decrypt the result of that with AES-GCM with the key we imported. We also need the IV, is that the octet string in the CMS structure?
```sh
    authEncryptedContentInfo: 
      contentType: pkcs7-data (1.2.840.113549.1.7.1)
      contentEncryptionAlgorithm: 
        algorithm: aes-256-gcm (2.16.840.1.101.3.4.1.46)
        parameter: SEQUENCE:
    0:d=0  hl=2 l=  17 cons: SEQUENCE          
    2:d=1  hl=2 l=  12 prim:  OCTET STRING      [HEX DUMP]:1CC5D17F5EBBC7C2F4428938
   16:d=1  hl=2 l=   1 prim:  INTEGER           :10
      encryptedContent: 
        0000 - b2 16 be 32                                    ...2
    authAttrs:
      <ABSENT>
    mac: 
      0000 - d2 3f 26 5f 8b 3d 63 17-b2 39 40 c4 4e 92 11 8d   .?&_.=c..9@.N...
    unauthAttrs:
      <ABSENT>
```
Does the MAC need to be added to the end?

Hang on, I got confused again. We're decrypting the key now, not the content, the contentEncryptionAlgorithm doesn't concern us right now. We need to decrypt encryptedKey with RSA. There's no IV for that.

[RsaOaepParams](https://developer.mozilla.org/en-US/docs/Web/API/RsaOaepParams) is just `{name: 'RSA-OAEP'}`.

```javascript
key = importPrivateKey(`[..pem without header or footer..]`);
encryptedKey = hexdump2ab(`[..encryptedKey hexdump..]`);

decrypted = await window.crypto.subtle.decrypt(
  {name: 'RSA-OAEP'}, key, encryptedKey);
```
`key` being the RSA key we imported before, encryptedKey being the result of hexdump2ab with the encryptedKey hexdump passed in.

The result is a 32-byte (256 bits) array buffer where we can't see the contents. To see the bytes, it can be converted to a Uint8Array with `new Uint8Array(decrypted)`. But we don't need to, we can just import the array buffer as a raw AES-GCM key.
```javascript
aesKey = await window.crypto.subtle.importKey(
  "raw", decrypted, {name: "AES-GCM"}, true, ["decrypt"]);
```
That was painless this time.

Finally, let's try to decrypt encryptedContent. Here I'm guessing I'm going to need to add the MAC at the end.
```javascript
encryptedContent = hexdump2ab(`        0000 - b2 16 be 32                                    ...2`)
mac = hexdump2ab(`      0000 - d2 3f 26 5f 8b 3d 63 17-b2 39 40 c4 4e 92 11 8d   .?&_.=c..9@.N...`)

both = new Uint8Array(encryptedContent.byteLength + mac.byteLength)
both.set(encryptedContent, 0)
both.set(mac, encryptedContent.byteLength)
```

Assuming the IV is the octet string above the encryptedContent in the CMS structure:
```javascript
    2:d=1  hl=2 l=  12 prim:  OCTET STRING      [HEX DUMP]:1CC5D17F5EBBC7C2F4428938
```
we're going to need a function to convert that to a byte array as well. It's not the same hex dump format.
```javascript
function hexcaps2ab(s) {
  const res = new Uint8Array(Math.ceil(s.length / 2));
  for (let i = 0; i < s.length; i += 2) {
    res[i/2] = Number.parseInt(s.slice(i, i+2), 16);
  }
  return res;
}
```

Trying to decrypt:
```javascript
iv = hexcaps2ab('1CC5D17F5EBBC7C2F4428938');

decryptedContent = new TextDecoder().decode(
  await window.crypto.subtle.decrypt(
    {name: 'AES-GCM', iv}, aesKey, both));
```

> "hi  
"

!!!

I did not expect it to be successful. I thought we would have problems with wrong padding or with how we're adding the MAC or something.

Trying to summarise the steps:

1. Get the decrypted version of the RSA private key (`openssl rsa -in key.pem -out keydecrypted.pem`) and convert to array buffer
2. Import it to WebCrypto with pkcs8 and usage "decrypt"
3. Get encryptedKey from `openssl cms -cmsout -print -in enc.bin` and convert to array buffer
4. Decrypt 3 with RSA and the key imported in 2
5. Import the resulting data as an AES-GCM key
6. Get encryptedContent from `openssl cms -cmsout -print -in enc.bin` and convert to array buffer
7. Get mac from `openssl cms -cmsout -print -in enc.bin` and convert to array buffer
8. Get the IV from `openssl cms -cmsout -print -in enc.bin` (OCTET STRING) and convert to array buffer
9. Combine 6 and 7 to one array buffer
10. Decrypt 9 with AES-GCM, key from 5, IV from 8

It feasibly could be automated, only needing `keydecrypted.pem` and `enc.bin` as input. Not `key.pem` because  you also need the password to get the decrypted RSA key out of it.

It's also possible when generating the RSA key to [specify `-nodes` to avoid encrypting it in the first place](https://stackoverflow.com/questions/5051655/what-is-the-purpose-of-the-nodes-argument-in-openssl).

Naïve Node script to automate this:
```javascript
#!/usr/bin/env node
// popenssl - decrypt openssl aes-gcm output with only the private key
// 2026-06-13 12:05
//
// Dependencies:
// - openssl
//
// Refs:
// - https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey
// - https://stackoverflow.com/a/78634900/18396947
// - https://javascript.info/regexp-lookahead-lookbehind
//
// Usage:
//     popenssl enc.bin key.pem
// where enc.bin should be openssl cipher output created with something like:
//     openssl cms -encrypt -aes-256-gcm -in input.txt -out enc.bin \
//      -recip req.pem -keyopt rsa_padding_mode:oaep -keyopt rsa_oaep_md:sha256
// (-aes-256-gcm and the keyopts to specify oaep and sha256 are required)
// and key should be a decrypted private key.
// private key can be created with:
//     openssl req -x509 -newkey rsa:2048 -keyout key.pem -out req.pem
// and decrypted with:
//     openssl rsa -in key.pem -out keydecrypted.pem
// or created without encryption to begin with by adding -nodes

const {execFileSync} = require('child_process');
const fs = require('node:fs');

function cmsDump(file) {
  return execFileSync(
    'openssl',
    ['cms', '-cmsout', '-print', '-in', file],
    { encoding: 'utf8' }
  );
}

function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}

async function pemImport(file) {
  const data = fs.readFileSync(file, 'utf8');

  const pemHeader = "-----BEGIN PRIVATE KEY-----";
  const pemFooter = "-----END PRIVATE KEY-----";

  const pemContents = data.replace(pemHeader, '').replace(pemFooter, '')//.replace(/\s+/g, '');

  if (data.length == pemContents.length) {
    throw new Error(`Key file does not contain PEM private key header and footer
(${pemHeader}, ${pemFooter}). If your key is encrypted, decrypt it first with:
openssl rsa -in key.pem -out keydecrypted.pem`);
  }

  // base64 decode the string to get the binary data
  const binaryDerString = atob(pemContents);
  // convert from a binary string to an ArrayBuffer
  const binaryDer = str2ab(binaryDerString);

  return crypto.subtle.importKey(
    "pkcs8", binaryDer, {name: "RSA-OAEP", hash: "SHA-256"}, true, ["decrypt"],
  );
}

// Input format:
//    0000 - d2 3f 26 5f 8b 3d 63 17-b2 39 40 c4 4e 92 11 8d   .?&_.=c..9@.N...
// (any string containing hex bytes in lowercase separated by space or hyphen)
function hexdump2ab(s) {
  const a = s.match(/(?<=(\s|-))[a-f0-9]{2}(?=(\s|-))/g);
  const res = new Uint8Array(Math.ceil(a.length));
  for (let i = 0; i < a.length; i++) {
    res[i] = Number.parseInt(a[i], 16);
  }
  return res;
}

// Input format:
// 1CC5D17F5EBBC7C2F4428938
// (string containing hex bytes in uppercase with no separation or junk)
function hexcaps2ab(s) {
  const res = new Uint8Array(Math.ceil(s.length / 2));
  for (let i = 0; i < s.length; i += 2) {
    res[i/2] = Number.parseInt(s.slice(i, i+2), 16);
  }
  return res;
}

function extractHexdumpSection(s, fieldName) {
  const lines = s.split('\n');
  const start = lines.findIndex(l => l.trim().startsWith(fieldName));
  if (start === -1) {
    throw new Error(`${fieldName} not found`);
  }
  const dump = [];
  for (let i = start + 1; i < lines.length; i++) {
    const line = lines[i];
    if (!/^\s*[0-9a-f]{4}\s+/.test(line))
      break;
    dump.push(line);
  }
  return hexdump2ab(dump.join('\n'));
}

function extractEncryptedKey(s) {
  return extractHexdumpSection(s, 'encryptedKey:');
}

function extractEncryptedContent(s) {
  return extractHexdumpSection(s, 'encryptedContent:');
}

function extractMac(s) {
  return extractHexdumpSection(s, 'mac:');
}

function extractIv(s) {
  const m = s.match(/OCTET STRING\s+\[HEX DUMP\]:([0-9A-F]+)/);
  if (!m.length) throw new Error('IV not found');
  return hexcaps2ab(m[1]);
}

args = process.argv.slice(2);
if (args.length != 2) {
  process.exitCode = 1;
  return console.log('Usage: popenssl enc.bin key.pem');
}

function importPrivateKey(pem) {
  const binaryDerString = atob(pem);
  const binaryDer = str2ab(binaryDerString);
  return crypto.subtle.importKey(
    "pkcs8", binaryDer, {name: "RSA-OAEP", hash: "SHA-256"}, true, ["encrypt"],
  );
}

async function getAesKey(cmsdump, pemfile) {
  // 1. Get the decrypted version of the RSA private key
  // 2. Import it to WebCrypto with pkcs8 and usage "decrypt"
  const rsaKey = await pemImport(pemfile);
  // 3. Get encryptedKey from CMS dump and convert to array buffer
  const encryptedKey = extractEncryptedKey(cmsdump);
  // 4. Decrypt 3 with RSA and the key imported in 2
  const decryptedKeyData = await crypto.subtle.decrypt(
    {name: 'RSA-OAEP'}, rsaKey, encryptedKey);
  // 5. Import the resulting data as an AES-GCM key
  return crypto.subtle.importKey(
    "raw", decryptedKeyData, {name: "AES-GCM"}, true, ["decrypt"]);
}

async function decryptContent(encfile, pemfile) {
  const cmsdump = cmsDump(encfile);
  const aesKey = await getAesKey(cmsdump, pemfile);
  // 6. Get encryptedContent from CMS dump and convert to array buffer
  const encryptedContent = extractEncryptedContent(cmsdump);
  // 7. Get mac from CMS dump and convert to array buffer
  const mac = extractMac(cmsdump);
  // 8. Get the IV from CMS dump (OCTET STRING) and convert to array buffer
  const iv = extractIv(cmsdump);
  // 9. Combine 6 and 7 to one array buffer
  const ciphertext = new Uint8Array(encryptedContent.byteLength + mac.byteLength)
  ciphertext.set(encryptedContent, 0)
  ciphertext.set(mac, encryptedContent.byteLength)
  // 10. Decrypt 9 with AES-GCM, key from 5, IV from 8
  return new TextDecoder().decode(
    await crypto.subtle.decrypt(
      {name: 'AES-GCM', iv}, aesKey, ciphertext));
}

(async () => {
  console.log(await decryptContent(args[0], args[1]));
})();
```
Parsing the CMS dump is not done safely.

And unless you use the keyopts that specify oaep and sha256 when generating the enc.bin, it will fail with:
```javascript
DOMException [OperationError]: The operation failed for an operation-specific reason
    at RSACipherJob.onDone (node:internal/crypto/util:670:19) {
  [cause]: [Error: error:02000079:rsa routines::oaep decoding error] {
    library: 'rsa routines',
    reason: 'oaep decoding error',
    code: 'ERR_OSSL_RSA_OAEP_DECODING_ERROR'
  }
}
```

One could extend this by working out from the CMS dump which SHA to use and support also 1, 384, and 512.

## Crypt clean filter
The next step is to create the `crypt` filter and corresponding command(s). In the section [Nonexistent filter](#nonexistent-filter) we set our `.gitattributes` to:
```conf
*.txt filter=crypt
```
Let's first unstage everything, because earlier when testing what a nonexistent filter would do, it staged the txt file unencrypted.
```sh
git reset
```

As we saw in the first section, other than the filter in `.gitattributes`, we need to define in the config the command it should run. `filter.crypt.clean` will run upon staging a file (where we want to encrypt the files in our case), and `filter.crypt.smudge` on pull or other updates to the files on disk. Let's only concern ourselves with `filter.crypt.clean` for now.
```sh
$ git config --global filter.crypt.clean crypt
```
And create a script called `crypt` in PATH
```sh
#!/usr/bin/env sh
echo "$@"
```

What's going to happen when I stage hi.txt?
```sh
$ git add hi.txt
```
Seemingly nothing. I didn't get the stdout. But in the staged file, instead of "hi" its contents are just an empty line. Not on disk; on disk it's still "hi".
```sh
$ cat hi.txt
hi
$ git reset
```

Presumably the script gets the path to each file as the first argument (`$1`) though, since in the first section we saw in the indent filter example `git config --global filter.indent.smudge cat` in order to keep the file as is on smudge. So I guess the stdout becomes the new contents of the file. Not on disk, only in the repository.

How to get OpenSSL to output the result of encryption to stdout instead of a file?

[SE 2019](https://stackoverflow.com/questions/56281320/how-to-use-openssl-to-output-encrypted-decrypted-message-to-stdout): It seems we can simply omit `-out` and pipe to `openssl base64`.

{% include note.html content='
> [!NOTE]
> Piping to `openssl base64` is actually unnecessary in our case, as I find out in [a later section](#conform-to-the-openssl-cms-output-format).
' %}

Attempt:
```sh
#!/usr/bin/env sh
[ -z "$1" ] && echo "no input file" && exit 1
openssl cms -encrypt -aes-256-gcm -in "$1" -recip req.pem \
        -keyopt rsa_padding_mode:oaep -keyopt rsa_oaep_md:sha256 |
    openssl base64
```
```sh
$ git add hi.txt
error: external filter 'crypt' failed 1
error: external filter 'crypt' failed
```
But it actually added it anyway (unencrypted "hi").

We don't receive any arguments then. We receive it as stdin?
```sh
#!/usr/bin/env sh
cat |
    openssl cms -encrypt -aes-256-gcm -recip req.pem \
            -keyopt rsa_padding_mode:oaep -keyopt rsa_oaep_md:sha256 |
    openssl base64
```

{% include note.html content='
> [!NOTE]
> This is a useless use of cat. The `cat |` at the start is not necessary.
' %}

```sh
$ git add hi.txt
$ git diff --cached
diff --git a/hi.txt b/hi.txt
new file mode 100644
index 0000000..2296844
--- /dev/null
+++ b/hi.txt
@@ -0,0 +1,19 @@
+TUlNRS1WZXJzaW9uOiAxLjAKQ29udGVudC1EaXNwb3NpdGlvbjogYXR0YWNobWVu
+dDsgZmlsZW5hbWU9InNtaW1lLnA3bSIKQ29udGVudC1UeXBlOiBhcHBsaWNhdGlv
+bi9wa2NzNy1taW1lOyBzbWltZS10eXBlPWF1dGhFbnZlbG9wZWQtZGF0YTsgbmFt
+ZT0ic21pbWUucDdtIgpDb250ZW50LVRyYW5zZmVyLUVuY29kaW5nOiBiYXNlNjQK
+Ck1JSUNCUVlMS29aSWh2Y05BUWtRQVJlZ2dnSDBNSUlCOEFJQkFER0NBYVF3Z2dH
+Z0FnRUFNRjB3UlRFTE1Ba0cKQTFVRUJoTUNRVlV4RXpBUkJnTlZCQWdNQ2xOdmJX
+VXRVM1JoZEdVeElUQWZCZ05WQkFvTUdFbHVkR1Z5Ym1WMApJRmRwWkdkcGRITWdV
+SFI1SUV4MFpBSVVJRU1wUUxzcktrR0tNaVdvVHp4a2pVOUpqdFF3T0FZSktvWklo
+dmNOCkFRRUhNQ3VnRFRBTEJnbGdoa2dCWlFNRUFnR2hHakFZQmdrcWhraUc5dzBC
+QVFnd0N3WUpZSVpJQVdVREJBSUIKQklJQkFHVDd6N2tnbk0rek1vSkRXOVhiZ09N
+TEhHK28xOGIzUTdET29xNHNpWVM2cVpWdnJtY3NCUktNVC92awpPT3JnVllBa3lV
+dDJqUXMzMFNyZlViaTNhUmlZeSt4MjZHajBDWjNkSHNsZG42eDQ4b0NFbjllMmR6
+MGtOT1RZClgvelA4bXlQcytuL3VnYmlBODlGZHF2dUF5T2tyZmUvdHVPOW12cmJ4
+THUxamxvN2s1RmwvcXZsYklXaWVzNTMKb2V2UlZIMGxpSkZEQXMwZUtaUGJlOUpn
+b25TTFBmak9sclk3TFlEMDBLS09hU3JQTmNPRDFGQU5WcTEreTZqaQpxL1JCU05s
+QVZTM2FjTERpdnI4STFYVDZRL2RiaUlQQlVzSTRHR3oydjgrTnJKR29xV0RkQUVY
+Y3k0REpvOFBxCldTMEJoU0ZaRmNGRVAxTHpuN1M2Ky81eVJSWXdNUVlKS29aSWh2
+Y05BUWNCTUI0R0NXQ0dTQUZsQXdRQkxqQVIKQkF3RXhGelBmYjFySjVSYXNNd0NB
+UkNBQk1GWWRCOEVFUERINENzbzFSSGtXUll6SERCYWMyVT0KCg==
```
That's massive when the content was just "hi", but I guess it contains the entire CMS structure with information about how it was encrypted, the encrypted key, the IV, the MAC, etc.

~~Actually I think it might have got applied multiple times? It shouldn't be as long as this. After trying again, it's just:~~
```sh
$ git diff --cached
diff --git a/hi.txt b/hi.txt
new file mode 100644
index 0000000..246612b
--- /dev/null
+++ b/hi.txt
@@ -0,0 +1,19 @@
+TUlNRS1WZXJzaW9uOiAxLjAKQ29udGVudC1EaXNwb3NpdGlvbjogYXR0YWNobWVu
+dDsgZmlsZW5hbWU9InNtaW1lLnA3bSIKQ29udGVudC1UeXBlOiBhcHBsaWNhdGlv
+bi9wa2NzNy1taW1lOyBzbWltZS10eXBlPWF1dGhFbnZlbG9wZWQtZGF0YTsgbmFt
```
Oups, no, I got confused, it's still the 19 lines as you can see in the `@@ -0,0 +1,19 @@`, we just only see the first three to begin with because depending on the size of the console window it pages it or not.

## Abort on filter error
It kind of sucks that you have to be careful. We've already seen if there is any error you end up with the unencrypted text. Additionally, if you don't `git reset` it's not going to change what got staged by adding it again; if I change the crypt script to just do `echo wofiejwiofj` and do `add hi.txt` again, it's not going to change unless I `git reset` before `git add hi.txt`.

Is there a way to make it abort staging if the filter errors?

It's not mentioned on the git book, but [git filters can be marked `required`](https://github.com/kynan/nbstripout/issues/191).

```sh
git config --global filter.crypt.required true
```
In the config it looks like:
```conf
[filter "crypt"]
	clean = crypt
	required = true
```

## Crypt smudge filter
Changing the script to:
```sh
#!/usr/bin/env sh

USAGE="Usage: crypt (encrypt|decrypt)"

[ -z "$1" ] && echo "$USAGE" && exit 1

case "$1" in
    encrypt)
        cat |
            openssl cms -encrypt -aes-256-gcm -recip req.pem \
                    -keyopt rsa_padding_mode:oaep -keyopt rsa_oaep_md:sha256 |
            openssl base64
        ;;
    decrypt)
        cat |
            openssl base64 -d |
            openssl cms -decrypt -recip req.pem -inkey keydecrypted.pem
        ;;
    *)
        echo "$USAGE" && exit 1
        ;;
esac
```
And the config:
```sh
git config --global --replace-all filter.crypt.clean 'crypt encrypt'
git config --global filter.crypt.smudge 'crypt decrypt'
```
The quotes are important, or it will only put the first word.

To test the smudge filter:
```sh
$ mv hi.txt hi-backup.txt
$ git checkout -- hi.txt
$ cat hi.txt
hi
```

## Conform to the OpenSSL CMS output format
In our OpenSSL CMS enc.bin output files, the format was:
```conf
MIME-Version: 1.0
Content-Disposition: attachment; filename="smime.p7m"
Content-Type: application/pkcs7-mime; smime-type=authEnveloped-data; name="smime.p7m"
Content-Transfer-Encoding: base64

MIICBQYLKoZIhvcNAQkQARegggH0MIIB8AIBADGCAaQwggGgAgEAMF0wRTELMAkG
A1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGEludGVybmV0
IFdpZGdpdHMgUHR5IEx0ZAIUIEMpQLsrKkGKMiWoTzxkjU9JjtQwOAYJKoZIhvcN
AQEHMCugDTALBglghkgBZQMEAgGhGjAYBgkqhkiG9w0BAQgwCwYJYIZIAWUDBAIB
BIIBADiwzeg2HgLGfW2HtNEWxeJDkFPi9Vz2jNQf9IuSUqlElV+uV/MV2CwuDGO3
edNAM2RTze9oLdsdvhPAIt4DHgfKVKF+ua2h/thp2LL5uIciDLyVhMGlTO4g8nku
0I9jFWNHevi+kPJEsetvkGKPM/gC1EKDa9Vu8YLxNeA0KcmY+GHrxrWGXCjGwQn1
F4pzxgyQNcZqk+Qi9X2GE6aA1n9Nv++2glv34kJzJSx1t6Mfqmc9UYGD1Uiu42F/
4Uh3LssDSbxB6uEvJZr+RgbjO5M3t/z+byGRSmdrWu7rFYYcPG6YM6jdv1xbuUXD
NX6E7kSPWUXy23ihNcsTF+jVX/owMQYJKoZIhvcNAQcBMB4GCWCGSAFlAwQBLjAR
BAwcxdF/XrvHwvRCiTgCARCABLIWvjIEENI/Jl+LPWMXsjlAxE6SEY0=


```
(Including the two empty lines at the end)

Whereas with our clean filter we get:
```conf
TUlNRS1WZXJzaW9uOiAxLjAKQ29udGVudC1EaXNwb3NpdGlvbjogYXR0YWNobWVu
dDsgZmlsZW5hbWU9InNtaW1lLnA3bSIKQ29udGVudC1UeXBlOiBhcHBsaWNhdGlv
bi9wa2NzNy1taW1lOyBzbWltZS10eXBlPWF1dGhFbnZlbG9wZWQtZGF0YTsgbmFt
ZT0ic21pbWUucDdtIgpDb250ZW50LVRyYW5zZmVyLUVuY29kaW5nOiBiYXNlNjQK
Ck1JSUNCUVlMS29aSWh2Y05BUWtRQVJlZ2dnSDBNSUlCOEFJQkFER0NBYVF3Z2dH
Z0FnRUFNRjB3UlRFTE1Ba0cKQTFVRUJoTUNRVlV4RXpBUkJnTlZCQWdNQ2xOdmJX
VXRVM1JoZEdVeElUQWZCZ05WQkFvTUdFbHVkR1Z5Ym1WMApJRmRwWkdkcGRITWdV
SFI1SUV4MFpBSVVJRU1wUUxzcktrR0tNaVdvVHp4a2pVOUpqdFF3T0FZSktvWklo
dmNOCkFRRUhNQ3VnRFRBTEJnbGdoa2dCWlFNRUFnR2hHakFZQmdrcWhraUc5dzBC
QVFnd0N3WUpZSVpJQVdVREJBSUIKQklJQkFHVDd6N2tnbk0rek1vSkRXOVhiZ09N
TEhHK28xOGIzUTdET29xNHNpWVM2cVpWdnJtY3NCUktNVC92awpPT3JnVllBa3lV
dDJqUXMzMFNyZlViaTNhUmlZeSt4MjZHajBDWjNkSHNsZG42eDQ4b0NFbjllMmR6
MGtOT1RZClgvelA4bXlQcytuL3VnYmlBODlGZHF2dUF5T2tyZmUvdHVPOW12cmJ4
THUxamxvN2s1RmwvcXZsYklXaWVzNTMKb2V2UlZIMGxpSkZEQXMwZUtaUGJlOUpn
b25TTFBmak9sclk3TFlEMDBLS09hU3JQTmNPRDFGQU5WcTEreTZqaQpxL1JCU05s
QVZTM2FjTERpdnI4STFYVDZRL2RiaUlQQlVzSTRHR3oydjgrTnJKR29xV0RkQUVY
Y3k0REpvOFBxCldTMEJoU0ZaRmNGRVAxTHpuN1M2Ky81eVJSWXdNUVlKS29aSWh2
Y05BUWNCTUI0R0NXQ0dTQUZsQXdRQkxqQVIKQkF3RXhGelBmYjFySjVSYXNNd0NB
UkNBQk1GWWRCOEVFUERINENzbzFSSGtXUll6SERCYWMyVT0KCg==
```

What happens if I omit the pipe to `openssl base64`? Raw data or the desired format which we're turning into base64 again unnecessarily?

The latter. It suffices to just change the script to remove the base64 bits.
```sh
#!/usr/bin/env sh

USAGE="Usage: crypt (encrypt|decrypt)"

[ -z "$1" ] && echo "$USAGE" && exit 1

case "$1" in
    encrypt)
        cat |
            openssl cms -encrypt -aes-256-gcm -recip req.pem \
                    -keyopt rsa_padding_mode:oaep -keyopt rsa_oaep_md:sha256
        ;;
    decrypt)
        cat |
            openssl cms -decrypt -recip req.pem -inkey keydecrypted.pem
        ;;
    *)
        echo "$USAGE" && exit 1
        ;;
esac
```

{% include note.html content='
> [!NOTE]
> Useless use of cat, the `cat |` is not necessary. Also instead of using a script you could just have the commands directly in the git config filter.
' %}

## Test repository
[plu5/git-filters-test](https://github.com/plu5/git-filters-test)

If you clone without req.pem and key.pem in cwd, it fails with:
```sh
Could not open file or uri for loading recipient certificate file from req.pem: No such file or directory
error: external filter 'crypt decrypt' failed 2
error: external filter 'crypt decrypt' failed
fatal: hi.txt: smudge filter crypt failed
warning: Clone succeeded, but checkout failed.
You can inspect what was checked out with 'git status'
and retry with 'git restore --source=HEAD :/'
```
and then we don't have hi.txt in the cloned repo:
```sh
$ ls -a
    .  ..  COPYING  crypt  .git  .gitattributes  README.md
```

Which can be resolved by copying over the req.pem and key.pem:
```sh
$ cp ../req.pem req.pem
$ cp ../key.pem key.pem
$ ls
COPYING  crypt  key.pem  README.md  req.pem
$ git restore --source=HEAD :/
$ ls
COPYING  crypt  hi.txt  key.pem  README.md  req.pem
$ cat hi.txt
hi
```

I thought to first set up a folder with req.pem and key.pem in it, then clone into pwd with `git clone https://github.com/plu5/git-filters-test .` in order to make it succeed from the start, but:
```sh
$ mkdir testclone
$ cd testclone
$ cp ../../req.pem req.pem
$ cp ../../key.pem key.pem
$ git clone https://github.com/plu5/git-filters-test .
fatal: destination path '.' already exists and is not an empty directory.
```

[SE question from 2010](https://stackoverflow.com/questions/2411031/how-do-i-clone-into-a-non-empty-directory). Looking at  Ken Williams' 2015 answer:
```sh
$ git clone --bare https://github.com/plu5/git-filters-test .
fatal: destination path '.' already exists and is not an empty directory.
```
So just the .git?
```sh
$ git clone --bare https://github.com/plu5/git-filters-test .git
$ git config --unset core.bare
```
But we only end up with the .git. What to do after to get the rest of the files?

`git reset --hard` does it, and does not get rid of our req.pem key.pem.
```sh
$ git reset --hard
HEAD is now at 7eceb4e Initial filter with explanation and one encrypted file
$ ls
COPYING  crypt  hi.txt  key.pem  README.md  req.pem
$ cat hi.txt
hi
```

I'll add these instructions to the readme.

## Trying to make it work in the browser
Although this set up works and all, my problem with it is it's not very portable, because we need openssl to add a new file with the same format (the encryption is standard, but how to obtain the same output format?), and we also need it to decrypt because we need the CMS dump.

Our enc.bin again:
```conf
MIME-Version: 1.0
Content-Disposition: attachment; filename="smime.p7m"
Content-Type: application/pkcs7-mime; smime-type=authEnveloped-data; name="smime.p7m"
Content-Transfer-Encoding: base64

MIICBQYLKoZIhvcNAQkQARegggH0MIIB8AIBADGCAaQwggGgAgEAMF0wRTELMAkG
A1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGEludGVybmV0
IFdpZGdpdHMgUHR5IEx0ZAIUIEMpQLsrKkGKMiWoTzxkjU9JjtQwOAYJKoZIhvcN
AQEHMCugDTALBglghkgBZQMEAgGhGjAYBgkqhkiG9w0BAQgwCwYJYIZIAWUDBAIB
BIIBAF3avLzfHPBbjUXHBhlfPZ7BkpX8cgbeJ1hLif6x/Oun2RqSl8Q+tz1kOjq6
OQIP0/1tLd5oPHiqf35v6206nu+RkIZ+/ShXouU4Vu/r2ScJ8is4zO+w/787yTgJ
wlNfFPfl5clctV59Q+/eBbzvKCij/l8JwLgZXuuiD+NBQEGLY3uXL7qMXyxWHdB3
NMf3uru38zrS/Es2IaxlIbPAl8yPPilgvf11x7LDmzHTprtjilq2ftlRwpqBqP0k
ETC8HiExjXKzwx0lJf37CB5COjUtH2uGcI/CRKNQFH5xKvedDdW9b/k2L4GZUUUs
8UJWZk9N2/TDwgxaQDlplWNL7zowMQYJKoZIhvcNAQcBMB4GCWCGSAFlAwQBLjAR
BAyOi3oba7vUOAUnYYgCARCABIGrKUcEECoegKc8bIgpZ8ycpzoahKQ=

```

The library pki.js has a class [EnvelopedData](https://pkijs.org/docs/api/classes/EnvelopedData/). There is this example in the docs to import CMS data:
```javascript
// Parse CMS Content Info
const cmsContent = pkijs.ContentInfo.fromBER(cmsContentRaw);
if (cmsContent.contentType !== pkijs.ContentInfo.ENVELOPED_DATA) {
  throw new Error("CMS is not Enveloped Data");
}
// Parse CMS Enveloped Data
const cmsEnveloped = new pkijs.EnvelopedData({ schema: cmsContent.content });
```
Not sure if will be possible to import our `authEnveloped-data` into it, but let's try.

First of all, what are you supposed to pass into `pkijs.ContentInfo.fromBER`? Not the string. According to the docs, it takes BufferSource "ASN.1 encoded raw data".

Maybe it's similar to how we imported the keys? I remember that in the [subtlecrypto importkey docs](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey), there is an example where they convert pem to binaryDer. But here it's BER.

> The original rule set was defined by the BER specification. CER and DER were developed later as specialized subsets of BER.  
—[Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/seccertenroll/distinguished-encoding-rules)

Let's just try to follow the same steps, then.
```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}

function str2der(s) {
  const binaryDerString = window.atob(s);
  return str2ab(binaryDerString);
}

pkijs = await import("https://cdn.jsdelivr.net/npm/pkijs@3/+esm");

cmsContent = pkijs.ContentInfo.fromBER(str2der(`[..base64..]`))
```

{% include note.html content='
> [!NOTE]
> If the base64 is missing any character, window.atob fails with "Uncaught DOMException: String contains an invalid character". I struggled with that for a bit.
>
> Also note that it\'s fine to have newlines in the input to window.atob (just send the string between grave characters), you don\'t need to repalce them yourself.
' %}

It seems to have worked. contentType 1.2.840.113549.1.9.16.1.23, authEnvelopedData. Because this is not 1.2.840.113549.1.7.3 (envelopedData), I think it will not allow me to create an EnvelopedData object with this.
```javascript
cmsEnveloped = new pkijs.EnvelopedData({schema: cmsContent.content});
```

> Uncaught Error: Object's schema was not verified against input data for EnvelopedData

Is [ContentInfo](https://pkijs.org/docs/api/classes/ContentInfo) enough for anything? Is it possible to obtain encryptedKey with it, for example?

It has a method `toJSON`.

`JSON.stringify(cmsContent.toJSON())`
```json
{
  "contentType": "1.2.840.113549.1.9.16.1.23",
  "content": {
    "blockName": "SEQUENCE",
    "blockLength": 500,
    "error": "",
    "warnings": [],
    "valueBeforeDecode": "[..long hex..]",
    "idBlock": {
      "blockName": "identificationBlock",
      "blockLength": 1,
      "error": "",
      "warnings": [],
      "valueBeforeDecode": "",
      "isHexOnly": false,
      "valueHex": "",
      "tagClass": 1,
      "tagNumber": 16,
      "isConstructed": true
    },
    "lenBlock": {
      "blockName": "lengthBlock",
      "blockLength": 3,
      "error": "",
      "warnings": [],
      "valueBeforeDecode": "",
      "isIndefiniteForm": false,
      "longFormUsed": true,
      "length": 496
    },
    "valueBlock": {
      "blockName": "ConstructedValueBlock",
      "blockLength": 496,
      "error": "",
      "warnings": [],
      "valueBeforeDecode": "[..long hex..]",
      "isIndefiniteForm": false,
      "value": [
        {
          "blockName": "INTEGER",
          "blockLength": 3,
          "error": "",
          "warnings": [],
          "valueBeforeDecode": "020100",
          "idBlock": {
            "blockName": "identificationBlock",
            "blockLength": 1,
            "error": "",
            "warnings": [],
            "valueBeforeDecode": "",
            "isHexOnly": false,
            "valueHex": "",
            "tagClass": 1,
            "tagNumber": 2,
            "isConstructed": false
          },
          "lenBlock": {
            "blockName": "lengthBlock",
            "blockLength": 1,
            "error": "",
            "warnings": [],
            "valueBeforeDecode": "",
            "isIndefiniteForm": false,
            "longFormUsed": false,
            "length": 1
          },
          "valueBlock": {
            "blockName": "IntegerValueBlock",
            "blockLength": 1,
            "error": "",
            "warnings": [],
            "valueBeforeDecode": "",
            "isHexOnly": false,
            "valueHex": "00",
            "valueDec": 0
          },
          "name": "",
          "optional": false
        },
[..]
}}}
```
It's super long and obtuse.

I know from the CMS dump that the encryptedKey hex begins with 38b0, and there are no results for it in the output. Nor b038, in case it was a different order or something.

Let's try with the lower-level library asn1.js.
```javascript
asn1js = await import("https://cdn.jsdelivr.net/npm/asn1js@3/+esm")

asn1 = asn1js.fromBER(str2der(`[..base64..]`))
JSON.stringify(asn1.result)
```

It's the same structure. Not the exact same output as it's now 50663 characters and previously it was 43421. But still no 38b0 in there.

I got stuck for ages, then realised I was using the wrong CMS dump. I thought I checked this, but I was blind. In the right one, encryptedKey actually starts with 5ddabcbcdf1cf05b, and not only is it is there in the JSON, it's there 12 times. The last result is in a OctetStringValueBlock, and it starts with it. There are three `OctetStringValueBlock`s:
- the first one is length 256 and contains encryptedKey,
- the second one is of length 12 and contains the IV,
- and the third one is of length 16 and contains the MAC.

That's almost everything. The last thing we need is encryptedContent. It is in a PrimitiveValueBlock. There is only one PrimitiveValueBlock.

That should be easy to extract, then. So we should be able to decrypt the CMS output in the browser.

We also need a way to be able to create it. Given that pki.js doesn't have an AuthEnvelopedData object, I'm guessing it will not be able to create this structure either.

[PKI.js examples: Working with CMS EnvelopedData](https://github.com/PeculiarVentures/PKI.js/blob/master/examples/HowToEncryptCMSviaCertificate/README.MD)

> Utilizing this object you can encode/encrypt a new CMS Enveloped Data message, decode/decrypt an existing one or add recipients to it.

I looked into whether there is a way to make OpenSSL use EnvelopedData with GCM, but it doesn't look like it. Maybe we could use CTR instead of GCM and add a MAC ourselves?

Another option is to not use OpenSSL. Since we can produce EnvelopedData with JavaScript, we could use the same thing in the git filter, using node to run it. Or forget about CMS and use JWE.

## Trying to recreate AuthEnvelopedData
I'm a little bit invested now, so I do want to at least try.

Let's try the naïve approach of just taking the object we have already and changing the IV, MAC, encryptedKey, and encryptedContent, and see if we can get OpenSSL to agree to decrypt the result. Where encryptedContent is encrypted with a new AES key, which is in turn encrypted with the RSA key we imported earlier to form encryptedKey. Actually the key we imported is the private key, not the public key, [but you can get the public key from the private one](https://security.stackexchange.com/questions/172274/can-i-get-a-public-key-from-an-rsa-private-key) with `openssl rsa -in keydecrypted.pem -pubout`.

Importing it (modified from the crypt script from earlier):
```javascript
function str2ab(str) {
  const buf = new ArrayBuffer(str.length);
  const bufView = new Uint8Array(buf);
  for (let i = 0, strLen = str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
}

async function pubImport(s) {
  const binaryDerString = window.atob(s);
  const binaryDer = str2ab(binaryDerString);
  return crypto.subtle.importKey(
    "spki", binaryDer, {name: "RSA-OAEP", hash: "SHA-256"}, true, ["encrypt"],
  );
}

// paste pem string between the graves
publicKey = await pubImport(``);
```
Note that for a public key we have to pass "spki" instead of "pkcs8", or it fails with "Data provided to an operation does not meet requirements". I also wasted more time debugging due to my terminal truncating the output, resulting in that same error.
(RSA-2048 SPKI base64: 390-400 characters)

Now for generating an AES-GCM key, encrypting text with it, encrypting the key with the public RSA key, and substituting encryptedKey, iv, encryptedContent, and mac. We don't even need to modify the asn1 object; all of the values are visible in the valueBeforeDecode hex string. We can substitute them and make a template to fill in with new values.

{% include note.html content='
> [!NOTE]
> This is not going to work, because ASN1 also has length fields, which are going to break when substituting in values of different lengths than the original had.
' %}

```javascript
template = "30820205060b2a864886f70d0109100117a08201f4308201f0020100318201a4308201a0020100305d3045310b30090603550406130241553113301106035504080c0a536f6d652d53746174653121301f060355040a0c18496e7465726e6574205769646769747320507479204c7464021420432940bb2b2a418a3225a84f3c648d4f498ed4303806092a864886f70d010107302ba00d300b0609608648016503040201a11a301806092a864886f70d010108300b060960864801650304020104820100{encryptedKey}303106092a864886f70d010701301e060960864801650304012e3011040c{iv}0201108004{encryptedContent}0410{mac}";

function ab2hex(a) {
  a = new Uint8Array(a);
  let res = '';
  for (const n of a) res = res.concat(n.toString(16).padStart(2, '0'));
  return res;
}

aesKey = await crypto.subtle.generateKey(
  {name: "AES-GCM", length: 256}, true, ["encrypt", "decrypt"]);
aesKeyRaw = await crypto.subtle.exportKey("raw", aesKey);
encryptedKey = ab2hex(await crypto.subtle.encrypt(
  {name: "RSA-OAEP"}, publicKey, aesKeyRaw));
template = template.replace("{encryptedKey}", encryptedKey);

content = "hello";
iv = window.crypto.getRandomValues(new Uint8Array(12));
template = template.replace("{iv}", ab2hex(iv));
encryptedContent = ab2hex(await crypto.subtle.encrypt(
  {name: "AES-GCM", iv}, aesKey, new TextEncoder().encode(content)));
template = template.replace("{encryptedContent}",
  encryptedContent.slice(0, -32));
template = template.replace("{mac}",
  encryptedContent.slice(-32, encryptedContent.length));
```

Now convert to base64 and give it the same header, and see if openssl agrees to decrypt this.
```javascript
function hex2base64(s) {
  const bytes = [];
  for (let i = 0; i < s.length; i += 2) {
    bytes.push(parseInt(s.substr(i, 2), 16));
  }
  return btoa(String.fromCharCode(...bytes));
}

res = `MIME-Version: 1.0
Content-Disposition: attachment; filename="smime.p7m"
Content-Type: application/pkcs7-mime; smime-type=authEnveloped-data; name="smime.p7m"
Content-Transfer-Encoding: base64

${hex2base64(template)}
`
```

I saved the result in a file called js.bin
```sh
$ openssl cms -decrypt -in js.bin -recip req.pem -inkey key.pem
Error reading SMIME Content Info
00A400BCF77F0000:error:0680008E:asn1 encoding routines:asn1_d2i_read_bio:not enough data:crypto/asn1/a_d2i_fp.c:272:
00A400BCF77F0000:error:0680006E:asn1 encoding routines:b64_read_asn1:decode error:crypto/asn1/asn_mime.c:149:
00A400BCF77F0000:error:068000CB:asn1 encoding routines:SMIME_read_ASN1_ex:asn1 parse error:crypto/asn1/asn_mime.c:533:
```

:-(

Let's modify the asn1 object instead. We need to figure out where the values we need are nested. Summarising what we found out about them earlier:
`OctetStringValueBlock`s:
- encryptedKey: `OctetStringValueBlock` length 256
- iv: `OctetStringValueBlock` length 12
- mac: `OctetStringValueBlock` length 16
- encryptedContent: `PrimitiveValueBlock`

Looking at the JSON nesting and verifying in the interpreter:
- asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[1].valueBlock.value[0].valueBlock.value[3].valueBlock : encryptedKey
- asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[1].valueBlock.value[1].valueBlock.value[0].valueBlock : iv
- asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[3].valueBlock (last valueBlock in the file) : mac
- asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[2].valueBlock (near the end of the file) : encryptedContent

asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[1] is enormous and makes up the majority of the file.

I'm not sure how to modify existing objects and there's no example for this in the [examples](https://asn1js.org/docs/examples/create-new-asn-structures-by-calling-constructors-with-parameters).

asn1.result has a method [toBER](https://asn1js.org/docs/api/classes/Sequence/#tober).

BaseStringBlock has a method [setValue](https://asn1js.org/docs/api/classes/BaseStringBlock#setvalue), but ValueBlock and OctetString don't, only getValue. The objects further up the nesting for our value blocks don't have that either. What they do have is:
- value : empty array
- valueBeforeDecode : empty ArrayBuffer, deprecated
- valueBeforeDecodeView : empty Uint8Array
- valueHex : ArrayBuffer
- valueHexView : Uint8Array

To test, I'm going to try to modify just the valueHex of encryptedContent, call asn1.result.toBER, feed this back in to asn1js.fromBER, and see what happens.
```javascript
asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[2].valueBlock.valueHex = new ArrayBuffer();
asn2 = asn1js.fromBER(asn1.result.toBER());
```
That worked. `asn2.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[2].valueBlock.blockLength` is 0 (was 4).

Let's do the same thing again then, but assigning the values on the objects instead of the template thing.
```javascript
aesKey = await crypto.subtle.generateKey(
  {name: "AES-GCM", length: 256}, true, ["encrypt", "decrypt"]);
aesKeyRaw = await crypto.subtle.exportKey("raw", aesKey);
encryptedKey = await crypto.subtle.encrypt({name: "RSA-OAEP"}, publicKey, aesKeyRaw);

content = "hello";
iv = window.crypto.getRandomValues(new Uint8Array(12));
encryptedContent = await crypto.subtle.encrypt(
  {name: "AES-GCM", iv}, aesKey, new TextEncoder().encode(content));

mac = encryptedContent.slice(-16);
encryptedContent = encryptedContent.slice(0, -16);

asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[1].valueBlock.value[0].valueBlock.value[3].valueBlock.valueHex = encryptedKey;
asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[1].valueBlock.value[1].valueBlock.value[0].valueBlock.valueHex = iv;
asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[3].valueBlock.valueHex = mac;
asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[2].valueBlock.valueHex = encryptedContent;
```

I guess to be able to use it in openssl, convert to base64 with the header again?
```javascript
function ab2base64(a) {
  return btoa(String.fromCharCode(...new Uint8Array(a)));
}

res = `MIME-Version: 1.0
Content-Disposition: attachment; filename="smime.p7m"
Content-Type: application/pkcs7-mime; smime-type=authEnveloped-data; name="smime.p7m"
Content-Transfer-Encoding: base64

${ab2base64(asn1.result.toBER())}
`
```

Pasting the result in js.bin again
```sh
$ openssl cms -decrypt -in js.bin -recip req.pem -inkey key.pem
Error decrypting CMS using private key
```

:-(

Let's put all the steps in one function and try again with new values to make sure it's not some kind of mismatch between initial cms structure and imported RSA key.
```javascript
async function make(cmsBase64, pubBase64) {
  function str2ab(str) {
    const buf = new ArrayBuffer(str.length);
    const bufView = new Uint8Array(buf);
    for (let i = 0, strLen = str.length; i < strLen; i++) {
      bufView[i] = str.charCodeAt(i);
    }
    return buf;
  }

  function str2der(s) {
    const binaryDerString = window.atob(s);
    return str2ab(binaryDerString);
  }

  asn1js = await import("https://cdn.jsdelivr.net/npm/asn1js@3/+esm");

  asn1 = asn1js.fromBER(str2der(cmsBase64));

  async function pubImport(s) {
    const binaryDerString = window.atob(s);
    const binaryDer = str2ab(binaryDerString);
    return crypto.subtle.importKey(
      "spki", binaryDer, {name: "RSA-OAEP", hash: "SHA-256"}, true, ["encrypt"],
    );
  }

  publicKey = await pubImport(pubBase64);

  aesKey = await crypto.subtle.generateKey(
    {name: "AES-GCM", length: 256}, true, ["encrypt", "decrypt"]);
  aesKeyRaw = await crypto.subtle.exportKey("raw", aesKey);
  encryptedKey = await crypto.subtle.encrypt({name: "RSA-OAEP"}, publicKey, aesKeyRaw);

  content = "hello";
  iv = window.crypto.getRandomValues(new Uint8Array(12));
  encryptedContent = await crypto.subtle.encrypt(
    {name: "AES-GCM", iv}, aesKey, new TextEncoder().encode(content));

  mac = encryptedContent.slice(-16);
  encryptedContent = encryptedContent.slice(0, -16);

  asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[1].valueBlock.value[0].valueBlock.value[3].valueBlock.valueHex = encryptedKey;
  asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[1].valueBlock.value[1].valueBlock.value[0].valueBlock.valueHex = iv;
  asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[3].valueBlock.valueHex = mac;
  asn1.result.valueBlock.value[1].valueBlock.value[0].valueBlock.value[2].valueBlock.value[2].valueBlock.valueHex = encryptedContent;

  function ab2base64(a) {
    return btoa(String.fromCharCode(...new Uint8Array(a)));
  }

  res = `MIME-Version: 1.0
Content-Disposition: attachment; filename="smime.p7m"
Content-Type: application/pkcs7-mime; smime-type=authEnveloped-data; name="smime.p7m"
Content-Transfer-Encoding: base64

${ab2base64(asn1.result.toBER())}
`

  return res;
}
```

Creating new things:
```sh
openssl req -x509 -newkey rsa:2048 -keyout keynew.pem -out reqnew.pem -nodes
echo "hi" | openssl cms -encrypt -aes-256-gcm -recip reqnew.pem -keyopt rsa_padding_mode:oaep -keyopt rsa_oaep_md:sha256 > encnew.bin
```
The base64 strings to pass to the make function are obtained with:
```sh
cat encnew.bin
openssl rsa -in keynew.pem -pubout
```

The result is pasted in js.bin. We try to decrypt with:
```sh
$ openssl cms -decrypt -in js.bin -recip reqnew.pem -inkey keynew.pem
hello
```

Nice

I wanted to see if i can get it working in the simplest way possible at first, hence the hardcoding the nestings (i.e. where encryptedKey and all the others are in asn1.result), but we can find them dynamically like this:
```javascript
function find(node, blockName, blockLength="unknown") {
  const block = node.valueBlock;
  if (!block) return;
  if (block.toJSON && block.toJSON().blockName == blockName) {
    if (blockLength == "unknown" || block.blockLength == blockLength) {
      return block;
    }
  }
  if (block.value) {
    for (const child of block.value) {
      const r = find(child, blockName, blockLength);
      if (r !== undefined) return r;
    }
  }
}

encryptedKeyObj = find(asn1.result, 'OctetStringValueBlock', 256);
ivObj = find(asn1.result, 'OctetStringValueBlock', 12);
macObj = find(asn1.result, 'OctetStringValueBlock', 16);
encryptedContentObj = find(asn1.result, 'PrimitiveValueBlock');
```
It's probably heavy calling toJSON that many times, but I'm not sure how else to get the block name.

## JWE
Using JWE instead of CMS may be simpler for our purposes. I also like the idea of being able to derive the key from a password and save just that, instead of having to save a really long RSA key. Simpler to store and typeable.

- [jose CompactEncrypt](https://github.com/panva/jose/blob/main/docs/jwe/compact/encrypt/classes/CompactEncrypt.md)
- [jose algorithm key requirements](https://github.com/panva/jose/issues/210#jwe-alg)

Like CMS, there are two algorithms: one for wrapping the key (alg) and one for encrypting the content (enc), where the latter is generated automatically, so we only have to provide a key for alg. alg doesn't have to be RSA, it can be AES-KW or AES-GCM-KW. Or even:

> PBES2-HS256+A128KW  
PBES2-HS384+A192KW  
PBES2-HS512+A256KW 	Secret or password of any length

Letting us provide a password directly, saving us from having to do the key derivation ourselves with WebCrypto.

Some notes on password choice in [this IETF 2015 memo](https://datatracker.ietf.org/doc/html/rfc7518).

> An ideal password is one that is as large as (or larger than) the
derived key length.  However, passwords larger than a certain
algorithm-specific size are first hashed, which reduces an attacker's
effective search space to the length of the hash algorithm.  It is
RECOMMENDED that a password used for "PBES2-HS256+A128KW" be no
shorter than 16 octets and no longer than 128 octets and a password
used for "PBES2-HS512+A256KW" be no shorter than 32 octets and no
longer than 128 octets long.

> Still, care needs to be taken in where and how password-based
encryption is used.  These algorithms can still be susceptible to
dictionary-based attacks if the iteration count is too small; this is
of particular concern if these algorithms are used to protect data
that an attacker can have indefinite number of attempts to circumvent
the protection, such as protected data stored on a file system.

ASCII is 1 octet (byte / 8 bits) per character, so 32-128 octets means 32-128 characters. And to give you an idea, the previous sentence is 89 characters long, so it's not that difficult to use a long enough password. It's certainly much shorter than an RSA key. It's also longer than an AES-256 key (unless you use the minimum recommended length, in which case it's equal), which is only 32 octets (256/8). Something meaningful, even if longer, would be faster to input, but would also have lower entropy and thus be weaker. And would also take longer to encrypt/decrypt because you also have to do the derivation. So maybe it's better to just use an AES key.

[There's also "dir"](https://security.stackexchange.com/questions/80966/what-is-the-point-of-aes-key-wrap-with-json-web-encryption) for alg, which means only doing the content encryption with the algorithm provided in enc with the key that you give. Just the one direct encryption, without key wrapping.

```javascript
jose = await import("https://cdn.jsdelivr.net/npm/jose@6/+esm");

key = await crypto.subtle.generateKey(
  {name: "AES-KW", length: 256}, true, ["wrapKey"]);

jwe = await new jose.CompactEncrypt(new TextEncoder().encode("hello"))
  .setProtectedHeader({alg: "A256KW", enc: "A256GCM"})
  .encrypt(key);
```
```text
eyJhbGciOiJBMjU2S1ciLCJlbmMiOiJBMjU2R0NNIn0.uwkqadObsi9CRYctdDy6g7rflj1GwaBZoKhgy1EnusdWj-tlnvUp1g.vT-7O3yMPZnsT38K.CzSmDmU.WnriFXhTCc2dsZlTCW0GEg
```
(header.encryptedkey.iv.ciphertext.mac)

With dir:
```javascript
key = await crypto.subtle.generateKey(
  {name: "AES-GCM", length: 256}, true, ["encrypt", "decrypt"]);

jwe = await new jose.CompactEncrypt(new TextEncoder().encode("hello"))
  .setProtectedHeader({alg: "dir", enc: "A256GCM"})
  .encrypt(key);
```
```text
eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2R0NNIn0..aCsEhMob3EAK5XDE.o9S4TS4.-_ypi0RPG7SQ4AWAefFEAg
```
(encryptedkey field is empty)

There's also [jose.generateSecret](https://github.com/panva/jose/blob/main/docs/key/generate_secret/functions/generateSecret.md) that can be used to generate the key instead of crypto.subtle.generateKey. For the resulting CryptoKey to be extractable, `{extractable: true}` must be passed as the second argument:
```javascript
key = await jose.generateSecret("A256GCM", {extractable: true});
jwe = await new jose.CompactEncrypt(new TextEncoder().encode("hello"))
  .setProtectedHeader({alg: "dir", enc: "A256GCM"})
  .encrypt(key);
```

[Decrypting](https://github.com/panva/jose/blob/main/docs/jwe/compact/decrypt/functions/compactDecrypt.md):
```javascript
res = await jose.compactDecrypt(jwe, key);
console.log(new TextDecoder().decode(res.plaintext));
```

Let's try with one of the algs that derive a key from a password:
```javascript
pass = new TextEncoder().encode("example password that is not long enough btw");

jwe = await new jose.CompactEncrypt(new TextEncoder().encode("hello"))
  .setProtectedHeader({alg: "PBES2-HS512+A256KW", enc: "A256GCM"})
  .encrypt(pass);

res = await jose.compactDecrypt(jwe, pass,
  {keyManagementAlgorithms: ["PBES2-HS512+A256KW"]});
console.log(new TextDecoder().decode(res.plaintext));
```
Pitfall here: PBES2 algorithms have to be explicitly allowed in DecryptOptions keyManagementAlgorithms, or compactDecrypt fails with « k: "alg" (Algorithm) Header Parameter value not allowed ».

> By default all "alg" (Algorithm) Header Parameter values applicable for the used key/secret are allowed except for all PBES2 Key Management Algorithms, these need to be explicitly allowed using this option.  
—[*panva/jose DecryptOptions*](https://github.com/panva/jose/blob/main/docs/types/interfaces/DecryptOptions.md)

## JWE CLI
For running JWE outside of the browser, there is [this](https://github.com/latchset/jose) C language implementation which is available on extra/jose on Arch Linux and comes with a CLI tool.

Generating a key and encrypting with the CLI tool:
```sh
$ jose jwk gen -i '{"alg": "A256GCM"}' -o oct.jwk
$ cat oct.jwk
{"alg":"A256GCM","k":"hCSSPmj6wLZUW2FfiKWsjfKoRkp6gffRTJmfztUkkdQ","key_ops":["encrypt","decrypt"],"kty":"oct"}
$ echo hi | jose jwe enc -I- -k oct.jwk --compact
eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2R0NNIn0..CwXYmfDzlULZZBsk.YZ7o.Z9KbJ52pv_sn5qLcr6XDNQ
```
Encrypting then decrypting:
```sh
$ echo hi | jose jwe enc -I- -k oct.jwk --compact | jose jwe dec -i- -k oct.jwk
hi
```

You could alternatively run a JavaScript script with node.

I also made a client-side JavaScript tool in [/tool/jwe](/tool/jwe) for encrypting and decrypting compact JWE ([GitHub](https://github.com/plu5/tool/tree/gh-pages/jwe)).

## JWE git filter
```sh
git config --global filter.crypt.clean 'jose jwe enc -I- -k oct.jwk --compact'
git config --global filter.crypt.smudge 'jose jwe dec -i- -k oct.jwk'
git config --global filter.crypt.required true
```
.gitattributes:
```conf
*.txt filter=crypt
```

That's all. It's a lot simpler.

{% include fin.html %}
