---
layout: post
title: 17 — Encryption with git clean/smudge filters
date: 2026-05-15 05:49
modified_date: 2026-05-17 06:03
categories: git cryptography
lang: en
redirect_from: /devlog/17
wip: true
---

## Git attributes
Smudge and clean filters are set in [`.gitattributes`](https://git-scm.com/book/en/v2/Customizing-Git-Git-Attributes) ([fr](https://git-scm.com/book/fr/v2/Personnalisation-de-Git-Attributs-Git)) (or `.git/info/attributes`), which I currently know only as "the thing controlling what happens to line endings". I personally prefer for line-endings to be kept as they are, so I have a `.gitattributes` with `* -text`. This is pretty obtuse [..] TODO. Git attributes can also be used to control what diff does for different file types, avoid diffing certain files altogether (mark as binary), [exclude files from export](https://coder-joey.github.io/Utilising-Git-Attributes/), substitute format strings in export (`export-subst`, I imagine this is rarely used), ignore or prioritise certains files in a merge, or transform file contents.

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

## OpenSSL CMS certificates
For generating a certificate, [openssl req](https://docs.openssl.org/master/man1/openssl-req/) is used.

```sh
openssl req -newkey rsa:2048 -keyout key.pem -out req.pem
```
have to type password 4-1024 characters, and optionally: 2-letter country name, state or province name, locality name, organisation name, organisational unit name, common name, email address, challenge password, optional company name.

`key.pem`:
```text
-----BEGIN ENCRYPTED PRIVATE KEY-------
[..long base64 hash..]
-----END ENCRYPTED PRIVATE KEY-----
```
`req.pem`
```text
-----BEGIN CERTIFICATE REQUEST-----
[..long base64 hash..]
-----END CERTIFICATE REQUEST-----
```

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

```sh
openssl cms -decrypt -in enc.bin -recip req.pem -inkey key.pem
```
Have to type the password we gave when creating the private key. Then it outputs the decrypted text to stdout.

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

rsaEncryption (1.2.840.113549.1.1.1) is used to encrypt the key. 1.2.840.113549.1.1.1 [is the OID](https://oid-base.com/get/1.2.840.113549.1.1.1) of `RSAES-PKCS1-v1_5`.

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

encrypted key could be the problem.
we also have to enter a password when we do anything with it

openssl pkcs8

and 1.2.840.113549.1.1.7 is maybe not sha-256

`-keyopt rsa_oaep_md:sha256`

{% include fin.html %}
