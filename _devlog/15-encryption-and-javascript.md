---
layout: post
title: 15 — Encryption and JavaScript
date: 2026-04-26 07:28
modified_date: 2026-05-13 16:57
categories: cryptography javascript webcrypto tool krita timelapsemod emacs emacs-doentry
lang: en
redirect_from: /devlog/15
wip: true
---

## Tool
If you're a GitHub user, you can create a repository with a branch named `gh-pages`, put an `index.html` file in there, and it will be automatically deployed and served on `username.github.io/reponame`. Any subdirectories in that respository can also have an `index.html` and they will be in the hierarchy: `username.github.io/reponame/subdirname` or even `username.github.io/reponame/subdirname/subsubdirname`. So I made a repository called [`tool`](https://github.com/plu5/tool) with a view to put in there JavaScript scripts that it would be useful to have access to when I am away.

## Web Crypto
One of these tools is for encryption and decryption. There is an API included with modern browsers called [Web Crypto](https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API). It's not the worst cryptography API in existence, but it's intentionally strict and obtuse. The errors amount to:

> A parameter or an operation is not supported by the underlying object

or:

> The operation failed for an operation-specific reason

in Firefox. In Chromium, there's not even that, it's just a DOMException with no description. The only way to figure out what's wrong is by trial and improvement.

## Disclaimer
This devlog discusses encryption, but I am no cryptographer and my words should be taken _with salt_.

## Algorithms available in Web Crypto
This may differ from browser to browser. I am using Firefox 149.

- RSA-OAEP
- AES-CTR
- AES-CBC
- AES-GCM

RSA is asymmetric (private and public key)

AES is symmetric (just one key)

AES is mathematically harder to break than RSA

authentication

## AES
AES is an algorithm that takes a block of 16 bytes and encrypts it. CTR/CBC/GCM (called "modes") define how to use AES, not only to be able to handle larger data, but even the handling for a single block differs.

There also exists ECB mode ("Electronic Codebook") which is the most naïve way of chaining that is not secure at all; the common example that is [often used in talks](https://youtu.be/3rmCGsCYJF8?t=502) is encrypting an image with it and the resulting image is still recognisable. It's not included with Web Crypto but is often included in cryptography libraries because it's useful to do low level manipulation (as we will see later).

The next in the order of simplicity/naïvety is CBC ("Cipher Block Chaining"). It's like ECB, except it does an XOR with an IV for each block. IV stands for initialisation vector, or colloquially, _salt_ (salt is actually a separate term that is used for storing passwords, but the concept is similar in that we're adding random data). The IV is XORed with the first block, then each subsequent block is XORed with the previous one. For 1 block, given an empty IV (=0), the result is the same as ECB.

What happens if the data doesn't divide neatly into blocks of 16 bytes? Padding. More on that [in a separate section](#padding).

The last two, CTR ("Counter Mode") and GCM ("Galois/Counter Mode"), are different in that they don't encrypt the input data directly, but rather the IV + "counter" (data that must be unique for each block, so usually a number that is incremented for each block is used). The result is called the "keystream". This is then XORed with the corresponding block of the plaintext, and the process is repeated for each block. With the last block, the XOR is done with a slice of the keystream the size of the last bit of data. Hence with these modes there is no need for padding.

Note that in Web Crypto, what is referred to as the counter is not separate from the IV. You pass the entire counter block, i.e. both the IV (a.k.a nonce) and counter, which must be 16 bytes (length of an AES block). It "splits" this with the counter making up the `length` last bits of the counter block. It doesn't get reset or anything, it increments from whatever was there in the counter block you gave. In GCM on the other hand, we only give the IV/nonce part (they recommend 12 bytes / 96 bits) and the bits to serve as the counter (remaining 4 bytes) are concatenated onto it to form the counter block.

We say that ECB and CBC are "block modes", and CTR and GCM are "stream modes".

GCM is the only AES mode here that has built-in "authentication". It does the same process as CTR, then adds a 16-bit tag to the end that is calculated from the data during encryption. During decryption it is recalculated, and if the result differs from the tag at the end of the ciphertext, it will refuse to decrypt (the usual "The operation failed for an operation-specific reason" in Web Crypto). However, because the ciphertext sans tag is produced in the same way as CTR, we can remove the tag and decrypt it with CTR. See section [AES: Decrypting GCM with CTR](#aes-decrypting-gcm-with-ctr).

A flaw that the other AES (non-GCM) modes share is that someone with access to the ciphertext can modify the plaintext, without needing the key, any knowledge of the plaintext, nor ability to decrypt. This is also true for one-time pads. See section [AES-CTR W1: Modifying the plaintext despite not being able to decipher](#aes-ctr-w1-modifying-the-plaintext-despite-not-being-able-to-decipher).

## Padding
With the block modes, there is a need for padding if the size of the plaintext in bytes is not a multiple of 16. In reality the padding policy is applied across the board, so even if the size is precisely 16 bytes it will be added (a whole block in this case; the encrypted message will be 32 bytes.)

- NoPadding
- ZeroPadding
- PKCS5Padding
- PKCS#7

Web Crypto forces PKCS#7 for the block mode it provides (which is only CBC), so if you use Web Crypto you do not need to read about the others, other than to satisfy your curiosity or understand what it means in other libraries / encryption tools.

NoPadding is just no padding. It will fail if the size of the message is not a multiple of 16 bytes.

ZeroPadding is to fill with bytes of value 0. This is the naïve approach to padding. While it will allow you to encrypt, when decrypting, unless you know the length, you can't tell where the data ends and padding begins.

[PKCS](https://en.wikipedia.org/wiki/PKCS) is a series of Public-Key Cryptography Standards that were published in the 90s by the company that created RSA. [PKCS#5](https://www.foo.be/docs/opensst/ref/pkcs/pkcs-5v2/pkcs5v2-0.pdf) page 11 describes how to apply padding to DES or RC2, two defunct block ciphers from the 70s and 80s respectively, that both had a block size of 64 bits (8 bytes), half of the AES block size. It is done by filling the message with _n_ bytes of value _n_, where _n_ is the number required to reach a multiple of 8. For example, if the message is 7 bytes, we add 01. If the message is 6 bytes, we add 02 02.

> The length in octets of the encoded message will be a multiple of eight and it will
be possible to recover the message M unambiguously from the encoded message.
(This padding rule is taken from RFC 1423 [3].)  
—_RSA Data Security, Inc. Public-Key Cryptography Standards (PKCS)_

[PKCS#7](https://ipsec.pl/files/ipsec/Archiwum/pkcs-7.pdf) p20 generalises this for any size block below 256 (256 in hex: 100, exceeding a byte).

So, in practice, when you see PKCS5Padding in the context of AES, it means the same thing as PKCS#7 padding.

Also came across [this explanation of the padding used in Web Crypto by Saptarshi Basu, 2018 (SE)](https://stackoverflow.com/a/53765954/18396947), who cites the API specification.

See also: [Padding oracle attack](https://en.wikipedia.org/wiki/Padding_oracle_attack)

Stream modes are preferred nowadays, and one of the reasons for it is issues with padding.

## Generating a key
In cryptography, keys are just data like any other, you can use any data/text. They may need to be of particular length, depending on the algorithm. In AES, they need to be 128, 192, or 256 bits long. The process for encryption for each one is slightly different, not so far as to require a separate implementation, but may be referred to separately as AES-128, AES-192, and AES-256.

[SE.crypto: Has AES-128 been fully broken? (2019)](https://crypto.stackexchange.com/questions/76738/has-aes-128-been-fully-broken)
(No. Attacks better than bruteforce have been found, but still take well beyond a realistic number of attempts. Despite this, 256 is usually the recommendation nowadays. [CNSSP-15](https://www.cnss.gov/CNSS/issuances/Policies.cfm) p11 says to use 256-bit keys with AES.)

In Web Crypto, keys are wrapped in `CryptoKey` objects with the following structure:
```javascript
{
  algorithm: Object { name: "AES-CTR", length: 256 },
  extractable: true,
  type: "secret",
  usages: Array [ "encrypt", "decrypt" ]
}
```
Generated with:
```javascript
await window.crypto.subtle.generateKey(
  {name: "AES-CTR", length: 256}, true, ["encrypt", "decrypt"]);
```
It hides the actual key, and associates it with a particular algorithm, particular usages, and is extractable or not (if not, it will not allow you to "export" the key, i.e. to extract the actual key). If you try to do something not allowed, like use this AES-CTR key to encrypt/decrypt a different algorithm, or use it to do something the usages don't allow, you will get one of the two obtuse errors I mentioned earlier. But you _can_ use the same key for different algorithms, it will just not allow you to with this higher level structure. What you can do is "export" it to get the actual key, and "import" it with different parameters.

## Exporting and importing keys
Exporting a key:
```javascript
let key = await window.crypto.subtle.generateKey(
  {name: "AES-CTR", length: 256}, true, ["encrypt", "decrypt"]);
const exported = await window.crypto.subtle.exportKey("jwk", key);
```
The first argument is the format to export it to: raw, pkcs8, spki, or jwk. pkcs8 and spki are not supported with AES (see [MDN information on supported formats](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey#supported_formats)). raw gives you an ArrayBuffer. jwk gives an object of this structure:
```javascript
{
  alg: "A256CTR",
  ext: true,
  k: "kgNpv6rT_TQDhcX5NLDX0OQU37e3x4Ok4Is31D7mMrFo",
  key_ops: Array [ "encrypt", "decrypt" ],
  kty: "oct"
}
```
`kty` stands for "key type", here `oct` (octet sequence) which is the type used for symmetrical keys like AES and HMAC (other possible values: `RSA` for RSA, `EC` "Elliptic Curve" for ECDSA and ECDH, `OKP` "Octet Key Pair" for Ed25519 and X25519). "Octet" is the French word for byte (for example, instead of GB we say Go), it's new to me that it's sometimes used in English too.

`k` is the actual key, stored in url-safe base64 (`-` `_` instead of `+` `/`, and no padding).

To import jwk, you could feed back all of the above into `importKey`, but in practice the only two absolutely necessary are `k` and `kty`.
```javascript
// Using CBC this time instead of CTR
key = await window.crypto.subtle.importKey(
  "jwk", {k: domkey.value, kty: "oct"}, "AES-CBC", true,
  ["encrypt", "decrypt"]);
```

Usages: encrypt, decrypt, ~~sign, verify, deriveKey, deriveBits,~~ wrapKey, unwrapKey.

{% include note.html content='
> [!NOTE]
> RSA-OAEP and AES-CTR/CBC/GCM don\'t work with sign, verify, deriveKey, and deriveBits. See [the table](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto#supported_algorithms) on the main SubtleCrypto MDN page.
' %}

Example exporting and importing raw:
```javascript
key = await window.crypto.subtle.generateKey(
  {name: "AES-CTR", length: 256}, true, ["encrypt", "decrypt"]);
exported = await window.crypto.subtle.exportKey("raw", key);
console.log(exported);  // -> ArrayBuffer { byteLength: 32 }
key = await window.crypto.subtle.importKey(
  "raw", exported, "AES-CBC", true,
  ["encrypt", "decrypt"]);
```
Oddly I found that I can give "jwk" instead of "raw" and pass a Uint8Array, and it still imports it without complaints.

I'm often in the situation where I have a key of one AES mode and I want to use it with another (here AES-CTR), so I do this one-liner to export and reimport:
```javascript
const key2 = await crypto.subtle.importKey("raw", await crypto.subtle.exportKey("raw", key), "AES-CTR", true, ["encrypt", "decrypt"]);
```

One line import base64 key:
```javascript
await window.crypto.subtle.importKey("jwk", {k: "Y2oHRyKp_3c7mZE6uwSUm_aInKLseAKXTllew6EL_o8", kty: "oct"}, "AES-CTR", true, ["encrypt", "decrypt"]);
```

## Deriving a key
Other than generating a key from nothing, there is also [a method to derive it](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveKey) from existing data. I will only talk about PBKDF2 ("Password-Based Key Derivation Function 2"), the others (HKDF, ECDH, X25519) are used in key agreement and expect high-entropy input like another key, rather than plain text. With PBKDF2 we can generate a key from normal text, to be able to encrypt/decrypt using something meaningful, like a password. This has the tradeoff of being a lot more expensive to calculate than generating a random key. [MDN example usage](https://github.com/mdn/dom-examples/blob/main/web-crypto/derive-key/pbkdf2.js) ([live](https://mdn.github.io/dom-examples/web-crypto/derive-key/)). They use 100000 iterations there. I first saw it in use in [this SE question (2024)](https://stackoverflow.com/questions/77962880/firefox-trigger-a-domexception-the-operation-failed-for-an-operation-specific) by Jason 'Slingshot' Miller, who also uses 100000.

> [A higher number of iterations] makes it more expensive for an attacker to run a dictionary attack against the keys. The general guidance here is to use as many iterations as possible, subject to keeping an acceptable level of performance for your application.  
—[MDN: Pbkdf2Params](https://developer.mozilla.org/en-US/docs/Web/API/Pbkdf2Params).

```javascript
async function deriveKey(pass, salt, alg) {
  const e = new TextEncoder();
  const prekey = await crypto.subtle.importKey(
    'raw', e.encode(pass), {name: 'PBKDF2'}, false, ['deriveKey']);
  const derivedkey = await crypto.subtle.deriveKey(
    {name: 'PBKDF2', salt: e.encode(salt), iterations: 100000, hash: 'SHA-256'},
    prekey, {name: alg, length: 256}, true, ['encrypt', 'decrypt']);
  return derivedkey;
}
```

For generating the salt, like generating the IV (as we will see later), random values. But not builtin JavaScript random because that's not considered cryptographically-secure randomness; Web Crypto comes with its own generator for random values. The salt can be of any size. MDN says it should be "at least 16 bytes". In both the MDN and Miller example they use 16.
```javascript
async function testDeriveKey
(pass, salt=window.crypto.getRandomValues(new Uint8Array(16)), alg='AES-CTR') {
  const key = await deriveKey(pass, salt, alg);
  // to see the resulting key in base64
  console.log((await window.crypto.subtle.exportKey("jwk", key)).k);
  return key;
}
```
```javascript
testDeriveKey('hi')
// -> zCQ7RrZl2oH9mi4drACRXY8m2qlGZOSXjr08bGCSqPo debugger eval code:5:11
testDeriveKey('hi')
// -> lwQt6W07pEH1EjsfSB5J4kTYF9XJ-VfmqGaoRj-omSc

// The same pass and salt result in the same key:
testDeriveKey('hi', 'hi')
// -> lC9gRu8i-vjZ6xNW50fwmaMLbsFVgQ6itXBJLrPty-c
testDeriveKey('hi', 'hi')
// -> lC9gRu8i-vjZ6xNW50fwmaMLbsFVgQ6itXBJLrPty-c
```
The key that was returned, not the base64 string but the CryptoKey object, will only be usable for encrypting/decrypting AES-CTR (and only AES-CTR, or you need to specify a different algorithm even if the raw key is the same, as I explained earlier in this section).

So how do you store the salt? In Miller's example he stores it with the encrypted text, separated by `:` (as well as the IV, more on that later).
```javascript
const encryptedText = `${uint8ArrayToBase64(encryptedData)}:${uint8ArrayToBase64(iv)}:${uint8ArrayToBase64(salt)}`;
```
Then in his decrypt function he extracts it like this:
```javascript
const [encryptedDataBase64, ivBase64, saltBase64] = encryptedText.split(':');
```

Something that confuses me is the `hash` parameter in the Pbkdf2Params. I saw [this SE answer by Maarten Bodewes, 2018](https://stackoverflow.com/a/48716151/18396947), which suggests that with SHA-512 the result is 512 bits:

> it is generally easier to derive the IV together with the key from the password and salt. If you use PBKDF2 with a SHA-512 hash this is easy: just take 128, 192 or 256 bits of AES key from the resulting hash and then take another 128 subsequent bits as IV and use that.

With `deriveKey` we can pass SHA-256, SHA-384, or SHA-512. But the resulting key is always the same length. I guess that it cuts the result automatically? Otherwise with anything other than SHA-256 the resulting key would not have been usable with AES.

## AES-CTR `counter` and `length`
Unlike the other modes, CTR takes `counter` instead of `iv`. The name of it is confusing, because it does not mean just the counter part, but the entire "counter block"—nonce+counter—that is passed through AES to produce the keystream. Therefore, it must be the length of an AES block (16 bytes). `length` determines the size in bits of the counter part of the counter block. Usually 64 is given, which results in:
```text
[ nonce (64) ] [ counter (64) ]
```
I sometimes see `length: 128`, which results in:
```text
[ nonce (0) ] [ counter (128) ]
```
What does it change in practice? Usually nothing, since we're passing the entire counter block anyway. In any case it just increments whatever you give it. `length` only changes how many times the counter could increment; the largest number representable in that number of bits. Web Crypto encrypt/decrypt operations check the input size and throw the usual "The operation failed for an operation-specific reason" if the `length` you gave is insufficient.

Relevant docs excerpts:

> counter  
An ArrayBuffer, a TypedArray, or a DataView — the initial value of the counter block. This must be 16 bytes long (the AES block size). The rightmost length bits of this block are used for the counter, and the rest is used for the nonce. For example, if length is set to 64, then the first half of counter is the nonce and the second half is used for the counter.

> length  
A Number — the number of bits in the counter block that are used for the actual counter. The counter must be big enough that it doesn't wrap: if the message is n blocks and the counter is m bits long, then the following must be true: n <= 2^m. The NIST SP800-38A standard, which defines CTR, suggests that the counter should occupy half of the counter block (see Appendix B.2), so for AES it would be 64.  
—*[MDN: AesCtrParams](https://developer.mozilla.org/en-US/docs/Web/API/AesCtrParams)*

## AES-CTR encryption/decryption script
The inputs/outputs we are dealing with: key, IV, plaintext, ciphertext.

In AES-CTR, the same key+IV combination must not be used for more than one message. If it is, this is considered a serious security vulnerability: if the plaintext for one of the messages is known, it is possible to decrypt the others without knowing the key (this is the same as reusing a [one-time pad](https://en.wikipedia.org/wiki/One-time_pad)). I demonstrate how in [a later section](#aes-ctr-decrypting-other-messages-that-were-encrypted-with-the-same-key-iv-combination).

Knowing this, I choose to have separate UI for encryption and decryption, and make the IV an output for encryption. Because if it's an input, this would encourage reusing the same key+IV combination. Then, to avoid inconvenience, we could have a button that would transfer down the output from the encryption to the UI for decryption.

`index.html`:
```html
<!DOCTYPE html>
<body>
  <pre id="err"></pre>
  <button id="clear">Clear</button>
  <textarea
    id="key"
    placeholder="Key (base64). Leave blank to generate one.">
  </textarea>

  <br/><br/>

  <textarea id="in" placeholder="Input (cleartext)"></textarea>
  <button id="encrypt">Encrypt</button>
  <textarea id="out" placeholder="Output (ciphertext, base64)"></textarea>
  <textarea id="iv" placeholder="IV (base64)"></textarea>

  <br/><br/>

  <textarea id="in2" placeholder="Input (cyphertext, base64)"></textarea>
  <button id="transfer">Transfer down the values from Encrypt output</button>
  <br/>
  <textarea id="iv2" placeholder="IV (base64)"></textarea>
  <button id="decrypt">Decrypt</button>
  <textarea id="out2" placeholder="Output (cleartext)"></textarea>

  <script src="crypt.js"></script>
</body>
</html>
```

`crypt.js`:
```javascript
const e = (id) => document.getElementById(id);

const domkey = e('key');
const domerr = e('err');
const domencrypt = {
  btn: e('encrypt'), in: e('in'), out: e('out'), iv: e('iv'),
};
const domdecrypt = {
  btn: e('decrypt'), in: e('in2'), out: e('out2'), iv: e('iv2'),
  transfer: e('transfer'),
};
const inputs = [domkey,
                domencrypt.in, domencrypt.out, domencrypt.iv,
                domdecrypt.in, domdecrypt.out, domdecrypt.iv];
const clearbtn = e('clear');

const clear = () => {
  for (let inp of inputs) {
    inp.value = '';
  }
};

const transfer = () => {
  domdecrypt.in.value = domencrypt.out.value;
  domdecrypt.iv.value = domencrypt.iv.value;
}

const genKey = async (alg='AES-CTR') => {
  const key = await window.crypto.subtle.generateKey(
    {name: alg, length: 256}, true, ['encrypt', 'decrypt']);;
  const exported = await window.crypto.subtle.exportKey('jwk', key);
  domkey.value = exported.k;
  return key;
};

const getKey = async (alg='AES-CTR') => {
  let key = null;
  if (domkey.value.length > 0) {
    try {
      key = await window.crypto.subtle.importKey(
        'raw', base64ToUint8Array(domkey.value), alg, true,
        ['encrypt', 'decrypt']);
    } catch (e) {
      domerr.textContent = `Unable to import key. ${e}`;
    }
  } else {
    key = await genKey(alg);
  }
  return key;
};

const encode = (text) => new TextEncoder().encode(text);
const decode = (text) => new TextDecoder().decode(text);

const uint8ArrayToBase64 = (array) => {  // url-safe variant of base64
  const r = btoa(String.fromCharCode(...new Uint8Array(array)));
  return r.replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
};
const base64ToUint8Array = (base64) => {
  return Uint8Array.from(atob(base64.replace(/-/g, '+').replace(/_/g, '/')),
                         c => c.charCodeAt(0));
};

const encrypt = async () => {
  const iv = window.crypto.getRandomValues(new Uint8Array(16));
  domencrypt.iv.value = uint8ArrayToBase64(iv);
  try {
    domencrypt.out.value = uint8ArrayToBase64(
      await window.crypto.subtle.encrypt(
        {name: 'AES-CTR', counter: iv, length: 64}, await getKey('AES-CTR'),
        encode(domencrypt.in.value)));
  } catch (e) {
    domerr.textContent = `Unable to encrypt. ${e}`;
  }
};

const decrypt = async () => {
  const iv = base64ToUint8Array(dom.iv.value);
  try {
    domdecrypt.out.value = decode(
      await window.crypto.subtle.decrypt(
        {name: 'AES-CTR', counter: iv, length: 64}, await getKey('AES-CTR'),
        base64ToUint8Array(dom.in.value)));
  } catch (e) {
    domerr.textContent = `Unable to decrypt. ${e}`;
  }
};

const decrypt = async () => {
  const iv = base64ToUint8Array(domdecrypt.iv.value);
  try {
    domdecrypt.out.value = decode(
      await window.crypto.subtle.decrypt(
        {name: 'AES-CTR', counter: iv, length: 64}, await getKey('AES-CTR'),
        base64ToUint8Array(domdecrypt.in.value)));
  } catch (e) {
    domerr.textContent = `Unable to decrypt. ${e}`;
  }
};

clearbtn.addEventListener('click', clear);
domencrypt.btn.addEventListener('click', encrypt);
domdecrypt.btn.addEventListener('click', decrypt);
domdecrypt.transfer.addEventListener('click', transfer);
```
Notes:
1. There is a single input for the key. The user can either provide their own, or leave it blank for one to be generated. It must be base64 of length 22/32/43 (128/192/256 bits respectively; base64 encodes 6 bits per character). Because I'm importing it as raw using `base64ToUint8Array`, both url-safe and non-url-safe base64 should work (see note 3 below) (otherwise jwk expects url-safe base64).
2. Re note 1: When we generate a key we always generate a 256bit key, but when importing it accepts any of the three AES lengths (128/192/256).
3. In `uint8ArrayToBase64`, after converting to base64 I also replace the characters `+` and `/`, and remove the padding characters `=`, in order for the output to be consistent with the jwk k format which is url-safe base64. I consulted [Ronnie Smith's 2024 SE answer](https://stackoverflow.com/a/78178053/18396947). There is no need to restore the padding characters to convert back to Uint8Array. `base64ToUint8Array` replaces `-` and `_` back to `+` and `/` before converting, and since non-url-safe base64 does not contain `-` or `_`, it should work with both url-safe and non-url-safe variants of base64.
4. Re notes 1 and 3: It does mean however that if a key is mixing and matching `+`, `/`, `-`, `_`, which would make it invalid base64, it would still be accepted, which may not be desirable.
5. Re note 1: I had the thought that if a key that does not meet the standards is used (for example not 256 bits, not base64) we could use deriveKey instead. I think it would be bad interface though, it should be explicit whether we are going to derive or use the key as is. It does mean some keys are not going to be accepted, but this is normal. I at least have the `err` element there so that the user will see when something goes wrong with importing the key without opening the console.
6. In the `encrypt` and `decrypt` functions, because we connect them to a button event they will pass to the functions we connect a `PointerEvent` object (which I don't use so I didn't put an argument, but if you do want to add arguments, the first one has to take in the event object).
7. Differences for AES-GCM:
   + When generating the IV, generate 12 bytes instead of 16. For AES-CTR it must be 16, for AES-GCM it can be any and it uses GHASH to make it into 12 bytes (see next section where this is discussed more in depth).
   + The parameters should be `{name: 'AES-GCM', iv}` (`iv` shorthand for `iv: iv` in JavaScript) instead of `{name: 'AES-CTR', counter: iv, length: 64}`. The parameter for the IV is named `iv` instead of `counter`.
   + The key need to be imported/generated as "AES-GCM"

## AES: Decrypting GCM with CTR
POC to run in the browser console. I didn't use const/let to be able to modify and rerun this easily without having to refresh.
```javascript
text = "A".repeat(100);
data = new TextEncoder().encode(text);
key = await crypto.subtle.generateKey({name: "AES-GCM", length: 256}, true, ["encrypt", "decrypt"]);
iv = crypto.getRandomValues(new Uint8Array(12));
encrypted = await crypto.subtle.encrypt({name: "AES-GCM", iv}, key, data);
decrypted = await crypto.subtle.decrypt({name: "AES-GCM", iv}, key, encrypted);
console.log("AES-GCM decrypted:", new TextDecoder().decode(decrypted));
// Convert the ArrayBuffer to Uint8Array to be able to manipulate it
encrypted = new Uint8Array(encrypted);
// Flip a bit
encrypted[0] ^= 1;
// AES-GCM will refuse to decrypt
try {
  decrypted = await crypto.subtle.decrypt({name: "AES-GCM", iv}, key, encrypted);
  console.log("AES-GCM tampered decrypted:", new TextDecoder().decode(decrypted));
} catch (e) {
  console.log("AES-GCM tampered decryption error:", e.message);
}
// Now try with CTR
try {
  key = await crypto.subtle.importKey("raw", await crypto.subtle.exportKey("raw", key), "AES-CTR", true, ["encrypt", "decrypt"]);
  // Remove the tag
  ciphertext = encrypted.slice(0, encrypted.length - 16);
  // Match initial counter conditions
  counter = new Uint8Array(16);
  counter.set(iv, 0);          // Place the IV (12 bytes) at the start
  counter.set([0,0,0,2], 12);  // The last 4 bytes are the counter which we start at 2
  decrypted = await crypto.subtle.decrypt({name: "AES-CTR", counter, length: 64}, key, ciphertext);
  console.log("AES-CTR tampered decrypted:", new TextDecoder().decode(decrypted));
} catch (e) {
  console.log("AES-CTR tampered decryption error:", e.message);
}
```

Output:
```text
AES-GCM decrypted: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AES-GCM tampered decryption error: The operation failed for an operation-specific reason
AES-CTR tampered decrypted: @AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
```

The result of the AES-CTR decryption will be gibberish if you don't match all the conditions. The key is easy enough, but the IV/counter is treated a bit differently in the two modes. I have already mentioned that in AES-CTR what is referred to as counter is not actually just the counter, it's also the IV, or nonce, or whatever you want to call it. Essentially it's the entire block of data which is passed through AES for each block of the keystream (with the counter incremented each time).
```text
[ nonce ] [ counter ]
```
In AES-GCM, what we pass in `iv` is just the nonce part, and it adds 4 bytes after it to serve as the counter. Which for some reason it starts at 2 (at least in the Web Crypto implementation). I only learned this from trial and improvement, as if I put 0 or 1 in there I get gibberish.

Another important thing to note is that the nonce+counter block has to be the length of one AES block; 16 bytes (because it's passed through AES to produce the keystream blocks). Unlike CTR, in GCM they allow you to pass an IV of an arbitrary size. When the IV not 12 bytes (16 bytes - 4 for the counter), it is passed through GHASH (same function used in computing the tag) to produce 12 bytes. So to be able to use the same IV in AES-CTR, the `iv` you pass to AES-GCM must be of length 12 bytes.

The `length` we give AES-CTR doesn't change the result. It only would if the counter were to wrap. Web Crypto does not allow it to wrap ("The operation failed for an operation-specific reason" if your `length` is too short for the size input you give).

(If wraps weren't guarded against, if you're just encrypting normally and the counter block wraps, nothing would happen visibly, it would just introduce the security issue that the same key+counterblock combination was reused, i.e. the same keystream block was reused, which breaks the encryption if a plaintext is known and even if it is not known opens the door to trying plausible plaintexts or statistical attacks. With our case here, we would also go out of alignment with GCM and the resulting hdecrypted text would be garbage.)

As a function taking the key as a Web Crypto key object, and the IV and and data to decrypt as Uint8Arrays:
```
const decryptGcmWithCtr = async (key, iv, data) => {
  const ciphertext = data.slice(0, data.length - 16);  // Snip tag
  // Match initial counter conditions
  const counter = new Uint8Array(16);
  counter.set(iv, 0);  // Place the IV (12 bytes) at the start
  counter[15] = 2;     // The last 4 bytes are the counter which we start at 2
  const decrypted = await crypto.subtle.decrypt(
    {name: "AES-CTR", counter, length: 64}, key, ciphertext);
  return new TextDecoder().decode(decrypted);
}
```
Note:
- The key must be AES-CTR (or Web Crypto will refuse to decrypt)
- The IV must be 12 bytes or the result will be gibberish, because otherwise AES-GCM passes it through GHASH to produce 12 bytes. We could get some kind of GHASH implementation here to produce the same IV in that case, but currently we don't so it will only work if the IV is 12 bytes.

[test: `await decryptGcmWithCtr(await window.crypto.subtle.importKey("jwk", {k: "Y2oHRyKp_3c7mZE6uwSUm_aInKLseAKXTllew6EL_o8", kty: "oct"}, "AES-CTR", true, ["encrypt", "decrypt"]), base64ToUint8Array("LVVSwYfVXSn9OAeh"), base64ToUint8Array("ql57HKg3j5njlkH5llNL0tdSXISz"))`]
[→ "hello"]

[there is [Uint8Array.fromBase64()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array/fromBase64) since 2015]

## A related confusion around JavaScript data types
- [ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
- TypedArray (Uint8Array, Int16Array, Float16Array, ...)
- DataView

Web Crypto returns from its functions (encrypt, decrypt, importKey as raw) an ArrayBuffer, which you can't subscript. To be able to manipulate it you have to convert it first, for example with `encrypted = new Uint8Array(encrypted);` as I had to do in the previous section.

If you do the wrong thing with types in JavaScript, you often get weird results with no error, so it can be a pain to debug.

## AES-CTR W1: Modifying the plaintext despite not being able to decipher
(W stands for weakness)
TODO

[i thought you can just add to the cleartext without knowing anything. or do you need to know a block? there is an example script here: https://www.seald.io/fr/blog/3-erreurs-courantes-quand-on-implemente-du-chiffrement]

## AES-CTR W2: Decrypting other messages that were encrypted with the same key+IV combination
TODO

[if you have two messages that were encrypted with the same key+IV and you XOR them. see also visual example in the seald article when you have messages of the same size and different blank portions]

## AES-CTR W3: Finding the IV given ciphertext, key, and partial plaintext
I came across [this Medium article](https://medium.com/@Espress0/cracking-aes-ctr-a-known-plaintext-decryption-walkthrough-8a46dec79594) by Vishwas S Adhikari, which is a CTF writeup. It demonstrates the following property of AES-CTR:
If the key is known and part of the plaintext is known, it is possible to find the IV.

This means if you forget or neglect to store the IV you can still decrypt, so long as you know some of the message and have the key and the ciphertext. This is not considered a vulnerability because the IV is not a secret and is often transmitted along with the ciphertext.

Questions:

1. How much of the message you have to know?
2. Do you have to know the position of your partial plaintext?

The easiest case is knowing the first block. An AES block is 16 bytes. This is not unlikely; we often do know the first block of the text; binary file types like images have headers (as demonstrated in the article), and plaintext files may have consistent markup at the top of each file. In ASCII (and in UTF-8 for characters in the ASCII block) each character is a byte, so it's the first 16 characters (including newlines, and if it has Windows line-endings then I guess it's 2 bytes per newline).

XML first 16 bytes is often something like: `<?xml version="1`

First 16 bytes of my devlogs: `---\nlayout: post`

Thoughts on the position: If we know partial plaintext, we can know whether we got the right IV or not by seeing when we try to decrypt with it if the result has the plaintext we know. So can't you keep trying every position? Which shouldn't really take that long.

We'll first just implement the most basic case, which is knowing the first block. The process is simple: XOR the first 16 bytes of the ciphertext with the first 16 bytes of the known plaintext, decrypt this with plain AES, and that's it, the result should be the IV.

Web Crypto offers one block mode, CBC. For 1 block, with IV 0, CBC does the same as ECB; just plain AES. However, Web Crypto requires PKCS#7 padding or it will refuse to accept it. Is there a trick to get it to accept it?

## Tricking Web Crypto to decrypt a single AES block with no padding
Yes; add what the padding would have looked like if it had been encrypted with it. Since we have a full block, the padding in this case would be 16 bytes of 16 (0x10), i.e. a whole second block of just padding. What CBC with IV 0 would have done to this second block is XOR it with the first block, then pass the result through AES. We can follow these same steps ourselves to obtain what the ciphertext would have been for the padding block.
```javascript
const decryptSingleAesBlock = async (key, block) => {
  const pad = new Uint8Array(16).fill(16);  // plaintext padding block
  // Find what the ciphertext of the padding block would have been
  const xor = new Uint8Array(16);
  for (let i = 0; i < 16; i++) {
    xor[i] = pad[i] ^ block[i];
  }
  const encryptedpad = await crypto.subtle.encrypt(
    {name: "AES-CBC", iv: new Uint8Array(16)},  // iv 0
    key, xor
  );
  // [ block ] [ encryptedpad ]
  const forged = new Uint8Array(32);
  forged.set(block.slice(0, 16), 0);
  forged.set(new Uint8Array(encryptedpad).slice(0, 16), 16);
  const decrypted = await crypto.subtle.decrypt(
    {name: "AES-CBC", iv: new Uint8Array(16)},
    key, forged
  );
  return decrypted;
}
```
After encryption `encryptedpad` is 32 bytes, because Web Crypto AES-CBC forces a padding block (again), so we have to slice it. I slice `block` too because why not, it's more explicit if nothing else.

(Why can't we use stream modes for this sort of thing? Because they don't encrypt/decrypt the input, but rather the IV+counter block, then XOR that with the input.)

See also [this 2020 SO question by KeyKi](https://stackoverflow.com/questions/60122638/web-crypto-api-throws-domexception-on-aes-decryption). He ended up using a library (AES-JS).

## Some notes about implementing AES block decryption anyway
{% include note.html content='
> [!NOTE]
> Some things here will likely be wrong, I am out of my depth. It is also incomplete.
' %}

Before figuring out how to recreate the padding, I had to do it this way, and I guess this devlog would be incomplete in any case without examining the actual algorithm. AES is a relatively simple algorithm to implement (though not to understand), but you should not use a hobbyist implementation like this for any real-life scenario. Making it function is easy enough, making it sufficiently consistent to avoid side-channel attacks is not. Modern processors have AES instructions built in ([AES-NI](https://en.wikipedia.org/wiki/AES_instruction_set)) which is a lot faster and more secure than what we can do in JavaScript.

AES inputs are:
1. key (16/24/32 bytes) (128/192/256 bits)
2. 16-byte block of data to encrypt/decrypt

(2)
The block of data is put into an array referred to as the `state`. This array, while 2D in memory:
```text
[s0, s1, s2, s3, s4, s5, ..., s15]
```
is conceptually a 4×4 matrix, where each byte is a cell:
```asciiart
 | s0  s4  s8  s12 |
 | s1  s5  s9  s13 |
 | s2  s6  s10 s14 |
 | s3  s7  s11 s15 |
```
We can access a cell with `state[row + 4*col]`. For example `s9` is at row 1 (0-based), column 2 (0-based): `state[1 + 4*2]` = `state[9]`.

Encryption involves transforming this matrix, and decryption reversing the transformation.

(1)
The transformation takes place over a number of rounds. This number is fixed based on the length of the key.
```asciiart
 Key Size | Rounds
 (bits)   | (number)
 ---------+---------
 128      | 10
 192      | 12
 256      | 14
```
The key is expanded such that each round can have its own 128-bit key (16 bytes), plus one more (added at the start of encryption / end of decryption). The expanded key is referred to as the key schedule.

Why more rounds for longer key sizes? The designers of Rijndael, the basis for AES, gave in their book the following reasons (which I personally find counter-intuitive) (not their exact words, I reworded):
- Longer keys increase the difficulty for brute force (for example, the number of tries for a 128-bit key are roughly 2¹²⁸ vs 2²⁵⁶ for 256). They do not want there to be any "shortcut attack", i.e. attack better than brute force. Since longer keys are much more difficult to brute force, the existence of attacks that take less attempts than that becomes more likely.
- Longer keys means more possibilities for known-key and related-key attacks.

The full quote can be found in [this crypto.SE answer by Ella Rose, 2019](https://crypto.stackexchange.com/a/68201).

It's a reversible process; the original key can be found from the key schedule by inverting the process. Key expansion is considered to be the weakest part of AES.

- [crypto.SE: Security importance of Key Schedule in Block Cipher (2017)](https://crypto.stackexchange.com/questions/45133/security-importance-of-key-schedule-in-block-cipher)
- [crypto.SE: Is the AES Key Schedule weak? (2012)](https://crypto.stackexchange.com/questions/1708/is-the-aes-key-schedule-weak)
- [crypto.SE: Why expand keys? Why not rather generate a longer key? (2022)](https://crypto.stackexchange.com/questions/100625/why-expand-keys-why-not-rather-generate-a-longer-key)
- [Wikipedia: AES key schedule](https://en.wikipedia.org/wiki/AES_key_schedule)
- [Braincoke: The AES Key Schedule explained](https://braincoke.fr/blog/2020/08/the-aes-key-schedule-explained/#aes-in-summary)

Steps in AES encryption:
- [A] add round key (first 16 bytes of key schedule)
- intermediate rounds (9 / 11 / 13)
  + [B] S-box substitution
  + [C] shift rows
  + [D] mix columns
  + [A] add round key (next 16 bytes)
- last round -- like intermediate rounds but without mix columns (because it would have been pointless as the purpose of it is diffusion to the next round and here there is no next round)
  + [B] S-box substitution
  + [C] shift rows
  + [A] add round key (last 16 bytes)

For decryption, the series of steps is reversed, and the functions (other than add round key) are inverse variants:
- reversed last round
  + [A] add round key (last 16 bytes)
  + [C⁻¹] inverse shift rows
  + [B⁻¹] inverse S-box substitution
- reversed intermediate rounds (9 / 11 / 13)
  + [A] add round key (previous 16 bytes)
  + [D⁻¹] inverse mix columns
  + [C⁻¹] inverse shift rows
  + [B⁻¹] inverse S-box substitution
- [A] add round key (first 16 bytes)

This is sometimes reordered to make it more symmetrical with the encryption steps (if you ignore the sections and look at just the actual steps, you'll notice it's the same, we're just changing what's in the loop):
- [A] add round key (last 16 bytes)
- reversed intermediate rounds (9 / 11 / 13)
  + [C⁻¹] inverse shift rows
  + [B⁻¹] inverse S-box substitution
  + [A] add round key (previous 16 bytes)
  + [D⁻¹] inverse mix columns
- reversed last round
  + [C⁻¹] inverse shift rows
  + [B⁻¹] inverse S-box substitution
  + [A] add round key (first 16 bytes)

We could even simplify this to a single loop:
- rounds (10 / 12 / 14)
  + [if first round:] [A] add round key / [otherwise:] [D⁻¹] inverse mix columns
  + [C⁻¹] inverse shift rows
  + [B⁻¹] inverse S-box substitution
  + [A] add round key

Resources consulted:
- [Sam Trenholme's webpage: The AES encryption algorithm](https://www.samiam.org/rijndael.html)
- [asecuritysite: AES Encryption](https://asecuritysite.com/subjects/chapter88)
- [Moserware: A Stick Figure Guide to the Advanced Encryption Standard (AES)](https://www.moserware.com/2009/09/stick-figure-guide-to-advanced.html)
  + [`Rijndael.cs`](https://github.com/moserware/AES-Illustrated/blob/master/Rijndael.cs)
  + [`SubstitutionBox.cs`](https://github.com/moserware/AES-Illustrated/blob/master/SubstitutionBox.cs)
  + [`FiniteFieldMath.cs`](https://github.com/moserware/AES-Illustrated/blob/master/FiniteFieldMath.cs)
  + [`Layers/ShiftRows.cs`](https://github.com/moserware/AES-Illustrated/blob/master/Layers/ShiftRows.cs)
  + [`Layers/MixColumns.cs`](https://github.com/moserware/AES-Illustrated/blob/master/Layers/MixColumns.cs)
  + [`KeySchedule.cs`](https://github.com/moserware/AES-Illustrated/blob/master/KeySchedule.cs)
- [`aes/core/_core.py` by donggeunkwon](https://github.com/donggeunkwon/aes/blob/master/aes/core/_core.py)
- [`micro_aes.c` by polfosol](https://github.com/polfosol/micro-AES/blob/master/micro_aes.c)
- [`mkcryptc/src/mk_aes.c` by MarekKnapek](https://github.com/MarekKnapek/mk_crypto/blob/mkcryptc/src/mk_aes.c)
- [Braincoke: The AES encryption algorithm explained](https://braincoke.fr/blog/2020/08/the-aes-encryption-algorithm-explained/)
  + [`aes/block.go`](https://github.com/Braincoke/cryptopals-solutions/blob/master/aes/block.go)
- [NIST FIPS 197: Advanced Encryption Standard](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf) (this is the main reference for AES, but it's terse)
- [Wikipedia: Advanced Encryption Standard](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) (all of the steps are explained with diagrams)
- [fr] [AES demonstration tool](https://www.utc.fr/~wschon/sr06/txPHP/) (explanations in French including history and mathematics background, and an interactive demonstration tool on the "Application" page)

> There is algebraic manipulation of polynomial representations of bytes on one side, efficient bitwise algorithms from real implementations on the other, and between the two, a chasm.  
—*On the difficulty in understanding AES transformations, me, a few weeks in*

## (A) Add round key
Let's look first at A, adding the round key. It's a good thing this comes first, because this is the easiest step to understand. The "round key" means the excerpt of the key schedule that will be used for this round, that in encryption starts with the first 16 bytes, and with decryption starts with the last 16 bytes. The way it is added is each byte is XORed.
```javascript
const addRoundKey = (s, rk) => {
  for (let i=0; i<16; i++) s[i] ^= rk[i];
};
```
The `s` here stands for state, and `rk` round key.

So at the start of encryption (and at the end of decryption) we would do:
```javascript
addRoundKey(s, rk.slice(0, 16));
```
In the intermediate rounds:
```javascript
addRoundKey(s, rk.slice(round*16, (round+1)*16));
```
For encryption start `round` at 1 and increment each iteration, for decryption start at 9 / 11 / 13 (or 10 / 12 / 14 if you don't have a separate final round) and decrement.

Generating the key schedule is [in a later section](#aes-key-expansion).

## (B) S-box substitution
B is S-box substitution. Each byte's value is substituted with another. In hobbyist implementations this is usually done via precomputed lookup tables, but this is vulnerable to cache timing attacks.

{% include note.html content='
> [!NOTE]
> Tables are not the only thing to worry about, there\'s also branches, early returns, anything that would have a different runtime for different inputs.
' %}

The use of such a step [dates back to DES](https://en.wikipedia.org/wiki/S-box), where, due to the cryptic nature of the S-box and initial refusal to reveal how it was derived, [people thought that the NSA hid backdoors in it](https://catless.ncl.ac.uk/Risks/6.01.html#subj4).

For AES, it is well-documented how the S-box was derived, and you can compute it yourself in real time.

This is what a precomputed AES S-box looks like, copied from the Wikipedia article [Rijndael S-box](https://en.wikipedia.org/wiki/Rijndael_S-box):
```asciiart
      00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
    +------------------------------------------------
 00 | 63 7c 77 7b f2 6b 6f c5 30 01 67 2b fe d7 ab 76
 10 | ca 82 c9 7d fa 59 47 f0 ad d4 a2 af 9c a4 72 c0
 20 | b7 fd 93 26 36 3f f7 cc 34 a5 e5 f1 71 d8 31 15
 30 | 04 c7 23 c3 18 96 05 9a 07 12 80 e2 eb 27 b2 75
 40 | 09 83 2c 1a 1b 6e 5a a0 52 3b d6 b3 29 e3 2f 84
 50 | 53 d1 00 ed 20 fc b1 5b 6a cb be 39 4a 4c 58 cf
 60 | d0 ef aa fb 43 4d 33 85 45 f9 02 7f 50 3c 9f a8
 70 | 51 a3 40 8f 92 9d 38 f5 bc b6 da 21 10 ff f3 d2
 80 | cd 0c 13 ec 5f 97 44 17 c4 a7 7e 3d 64 5d 19 73
 90 | 60 81 4f dc 22 2a 90 88 46 ee b8 14 de 5e 0b db
 a0 | e0 32 3a 0a 49 06 24 5c c2 d3 ac 62 91 95 e4 79
 b0 | e7 c8 37 6d 8d d5 4e a9 6c 56 f4 ea 65 7a ae 08
 c0 | ba 78 25 2e 1c a6 b4 c6 e8 dd 74 1f 4b bd 8b 8a
 d0 | 70 3e b5 66 48 03 f6 0e 61 35 57 b9 86 c1 1d 9e
 e0 | e1 f8 98 11 69 d9 8e 94 9b 1e 87 e9 ce 55 28 df
 f0 | 8c a1 89 0d bf e6 42 68 41 99 2d 0f b0 54 bb 16
```
For a byte of value 0xf2 for example, go to row f0 and column 02 and we get 0x89. To get back the original value:
```asciiart
      00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
    +------------------------------------------------
 00 | 52 09 6a d5 30 36 a5 38 bf 40 a3 9e 81 f3 d7 fb
 10 | 7c e3 39 82 9b 2f ff 87 34 8e 43 44 c4 de e9 cb
 20 | 54 7b 94 32 a6 c2 23 3d ee 4c 95 0b 42 fa c3 4e
 30 | 08 2e a1 66 28 d9 24 b2 76 5b a2 49 6d 8b d1 25
 40 | 72 f8 f6 64 86 68 98 16 d4 a4 5c cc 5d 65 b6 92
 50 | 6c 70 48 50 fd ed b9 da 5e 15 46 57 a7 8d 9d 84
 60 | 90 d8 ab 00 8c bc d3 0a f7 e4 58 05 b8 b3 45 06
 70 | d0 2c 1e 8f ca 3f 0f 02 c1 af bd 03 01 13 8a 6b
 80 | 3a 91 11 41 4f 67 dc ea 97 f2 cf ce f0 b4 e6 73
 90 | 96 ac 74 22 e7 ad 35 85 e2 f9 37 e8 1c 75 df 6e
 a0 | 47 f1 1a 71 1d 29 c5 89 6f b7 62 0e aa 18 be 1b
 b0 | fc 56 3e 4b c6 d2 79 20 9a db c0 fe 78 cd 5a f4
 c0 | 1f dd a8 33 88 07 c7 31 b1 12 10 59 27 80 ec 5f
 d0 | 60 51 7f a9 19 b5 4a 0d 2d e5 7a 9f 93 c9 9c ef
 e0 | a0 e0 3b 4d ae 2a f5 b0 c8 eb bb 3c 83 53 99 61
 f0 | 17 2b 04 7e ba 77 d6 26 e1 69 14 63 55 21 0c 7d
```
0x89 → row 80 and column 09 → 0xf2.

Programmatically we just put these values in a 1D array and each byte can be substituted for its value by a simple index lookup (`sbox[s[i]]`), since the indices cover every possible value for a byte; there are 16×16 = 256 values.

It looks like this in code (we just substitute each of the bytes):
```javascript
const sbox = Uint8Array.from([
  0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
  0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
  0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
  0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
  0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
  0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
  0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
  0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
  0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
  0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
  0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
  0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
  0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
  0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
  0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
  0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16
]);

function subbytes(s) {
  c = s.slice();  // copy
  for (let i=0; i<16; i++) c[i] = sbox[s[i]];
  return c;
};
```
It's the same process for the inverse function except with an inverse S-box.

For how the S-box is generated, I defer to [Sam Trenholme](https://www.samiam.org/s-box.html):

> The S-box is generated by determining the multiplicative inverse for a given number in Rijndael's Galois field. The multiplicative inverse is then transformed using the following affine transformation matrix:

```asciiart
 1 0 0 0 1 1 1 1
 1 1 0 0 0 1 1 1
 1 1 1 0 0 0 1 1
 1 1 1 1 0 0 0 1
 1 1 1 1 1 0 0 0
 0 1 1 1 1 1 0 0
 0 0 1 1 1 1 1 0
 0 0 0 1 1 1 1 1
```

He goes on to describing the series of steps to perform this programmatically.

Implementation in JavaScript, adapted from Trenholme's C implementation:
```javascript
function sbox(in) {
  let s = x = gmulInverse(in);
  for (let c = 0; c < 4; c++) {
    // One bit circular rotate to the left
    s = (s << 1) | (s >> 7);
    // XOR with x
    x ^= s;
  }
  // After the affine transformation, XOR the value by the 99 (0x63)
  x ^= 99;
  return x;
}
```
(`c` is not used? I guess it's just to repeat that 4 times?)

[His gmul inverse implementation](https://www.samiam.org/galois.html) uses a precalculated log table, which is vulnerable to cache timing attacks.

I found [this cool webpage](https://public.hochschule-trier.de/~knorr/AES/index.php) with an S-box generated dynamically. If you click on each of the value it has a breakdown of its calculation. You can also take the URL [public.hochschule-trier.de/~knorr/AES/index.php?xy=00](https://public.hochschule-trier.de/~knorr/AES/index.php?xy=40) and put any byte in place of the 00. Sadly it's implemented server-side so you can't look at the source.

He does give us this hint:

> The calculation [of the inverse] is made with the Extended Euclidean algorithm. Instead of normal division and multiplication you need to use Polynomialdivision and Polynomialmultiplication.

And he also links to [FIPS 197](https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf), though a different, I think older, version of the document. This version seems to have more details than the other I linked before for the calculation of the multiplicative inverse. p11 has:

> For any non-zero binary polynomial b(x) of degree less than 8, the multiplicative inverse of b(x), denoted b⁻¹(x), can be found as follows: the extended Euclidean algorithm [7] is used to compute polynomials a(x) and c(x) such that  
b(x)a(x) + m(x)c(x) = 1.

> Hence, a(x) • b(x) mod m(x) = 1, which means  
b⁻¹(x) = a(x) mod m(x).

where m(x) is the "irreductible polynomial" given in the previous page; x⁸+x⁴+x³+x+1. In hex this is is represented as 0x011b, because the hex numbers are represented as polynomials like this:
```asciiart
      x⁷ + x⁶ + x⁵ + x⁴ + x³ + x² + x + 1
 = 0b 1    1    1    1    1    1    1   1
 = 0xFF
 = 255
```
The x⁸ exceeds 1 byte, it's 0x100, then 0x1b is 0b00011011 which is:
```asciiart
      0x⁷ + 0x⁶ + 0x⁵ + 1x⁴ + 1x³ + 0x² + 1x + 1
 = 0b 0     0     0     1     1     0     1    1
 = 0x1B
 = 27
```

At first, I thought the:

b⁻¹(x) = a(x) mod m(x)

meant we simply have to calculate a(x) mod m(x), but obviously it's not so simple. What we're trying to find is not b⁻¹(x), but a⁻¹(x).

I don't have the mathematics background to understand why that is, but both the interactive S-box webpage and FIPS 197 tell us that to find b(x) that satisfies the equation above, we have to do the [_Extended Euclidean algorithm_](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm). That Wikipedia article has the following algorithm in pseudocode:
```ada
function inverse(a, p)
    t := 0;     newt := 1
    r := p;     newr := a

    while newr ≠ 0 do
        quotient := r div newr
        (r, newr) := (newr, r − quotient × newr)
        (t, newt) := (newt, t − quotient × newt)

    if degree(r) > 0 then 
        return "Either p is not irreducible or a is a multiple of p"

    return (1/r) × t
```
where `a` is the polynomial whose multiplicative inverse we are trying to find, and p is the irreductible polynomial x⁸+x⁴+x³+x+1. It is followed by an example for finding the multiplicative inverse of x⁶+x⁴+x+1 in GF(2⁸), exactly our case.

They note:

> Let us recall that in fields of order 2^n, one has −z = z and z + z = 0 for every element z in the field). Since 1 is the only nonzero element of GF(2), the adjustment in the last line of the pseudocode is not needed.

GF(2⁸), as [Moserware] explains in layman terms, is like another world with a different mathematical rules. Addition and subtraction become XOR operations. The only coefficients we deal with are 0 and 1. What happens to multiplication? TODO

Another thing to note is their `div` means division where you throw away the remainder. For example, `x div x+1` is equal just 1.

Manually applying the algorithm on x⁶+x⁴+x+1 (= 0b01010011 = 0x53):
```asciiart
 t = 0, newt = 1
 r = x⁸+x⁴+x³+x+1, newr = x⁶+x⁴+x+1
 
 (iteration 1)
 quotient = x⁸+x⁴+x³+x+1 div x⁶+x⁴+x+1
 
                                   x²  +1
           +-----------------------------
 x⁶+x⁴+x+1 | x⁸+0x⁷+0x⁶+0x⁵+x⁴+x³+0x²+x+1
             x⁸     +x⁶       +x³+x²
             ----------------------
                     x⁶    +x⁴   +x² +x+1
                     x⁶    +x⁴       +x+1
                     --------------------
                                  x²
 
 quotient = x²+1
 r = x⁶+x⁴+x+1, newr = x⁸+x⁴+x³+x+1-(x²+1)(x⁶+x⁴+x+1)
                     = x⁸+x⁴+x³+x+1-(x²(x⁶+x⁴+x+1)+1(x⁶+x⁴+x+1))
                     = x⁸+x⁴+x³+x+1-(x⁸+x⁶+x³+x²+x⁶+x⁴+x+1)
                     = x⁸+x⁴+x³+x+1-(x⁸+x³+x²+x⁴+x+1)
                     = x²
 t = 1, newt = 0-(x²+1) = x²+1
 
 (iteration 2)
 quotient = x⁶+x⁴+x+1 div x²
 
             x⁴     +x²
    +----------------------
 x² | x⁶+0x⁵+x⁴+0x³+0x²+x+1
      x⁶
      --
             x⁴        +x+1
             x⁴
             --
                        x+1
 
 quotient = x⁴+x²
 r = x², newr = x⁶+x⁴+x+1-(x⁴+x²)x²
              = x⁶+x⁴+x+1-(x⁶+x⁴)
              = x+1
 t = x²+1, newt = 1-(x⁴+x²)(x²+1)
                = 1-(x⁴(x²+1)+x²(x²+1))
                = 1-(x⁶+x⁴+x⁴+x²)
                = x⁶+x²+1
 
 (iteration 3)
 quotient = x² div x+1
 
           x+1
     +--------
 x+1 | x²+0x+0
       x² +x
       -----
           x
           x+1
           ---
             1
 
 quotient = x+1
 r = x+1, newr = x²-(x+1)(x+1)
               = x²-(x(x+1)+1(x+1))
               = x²-(x²+x+x+1)
               = x²-(x²+1)
               = 1
 t = x⁶+x²+1, newt = x²+1-(x+1)(x⁶+x²+1)
                   = x²+1-(x(x⁶+x²+1)+1(x⁶+x²+1))
                   = x²+1-(x⁷+x³+x+x⁶+x²+1)
                   = x⁷+x⁶+x³+x
 
 (iteration 4)
 quotient = x+1 div 1 = x+1
 r = 1, newr = x+1-(x+1)
             = 0
 t = x⁷+x⁶+x³+x
 
 newr is 0 so we stop
 no need to calculate newt since we're not going further
 
 => x⁷+x⁶+x³+x
    which is 0b11001010 = 0xCA
```
Notes:
- Instead of addition and subtraction you XOR, even in the long division
- [Polynomial long division](https://en.wikipedia.org/wiki/Polynomial_long_division)
- When I'm doing `r = something, newr = r-something`, it's assignment that happens in parallel, i.e. the value of `r` used in the second assignment is what it was _before_, not what we just assigned it to.
- It took me several attempts, kept making a little manipulation error on some step which then fucks up everything that follows.

[public.hochschule-trier.de/~knorr/AES/index.php?xy=53](https://public.hochschule-trier.de/~knorr/AES/index.php?xy=53)

How to go from this to an algorithm with bitwise operations like it's done in real implementations?

In his [`SubstitutionBox.cs`](https://github.com/moserware/AES-Illustrated/blob/master/SubstitutionBox.cs), Moserware does `FiniteFieldMath.F(FiniteFieldMath.G(currentByte))`

[`FiniteFieldMath.cs`](https://github.com/moserware/AES-Illustrated/blob/master/FiniteFieldMath.cs)

F:

> Calculates f(a), which is an affine transform ([..] it's a matrix multiply followed by a vector add).

(he's unfortunately one of those people who confuse e.g. and i.e., so I removed that from the quote)

G:

> Computes g(a) which is the inverse in the Rijndael field such that g(a) * a = 1.

That's the multiplicative inverse.

He does it by brute force. To find the multiplicative inverse of `a`, multiplying 1-255 by `a` in a loop until the result is 1.

I want to understand how to do it properly.

[I got stuck in this section for a long time, and stopped and moved to other sections. Working on key expansion helped clarify some things, because it similarly involves a gap between the algebra and the bitwise implementation, but with some much simpler operations (like Rcon). So I'm splitting this section into two in order to talk about key expansion first, even though it's a huge section, and even though key expansion necessitates S-box substitution, because in the rest of (B) I suddenly know more things that I learned from working on that other section, and it would be artificial to continue it here. In any case, we already do have a working implementation of `subbytes`, the one that uses table S-box lookups. In [the next part of (B)](#cont-b) is more faffing about trying to derive the S-box in real time.]

## AES key expansion
(or, creating the key schedule)

We need a key the size of an AES block (16 bytes = 128 bits) for each one of the rounds plus initial round.

`(numrounds+1)*128` bits

- [AES-128] 128-bit key (Nk = 4) needs to be expanded to (10+1)*128 = 1408 bits (176 bytes, 44 words)
- [AES-192] 192-bit key (Nk = 6) needs to be expanded to (12+1)*128 = 1664 bits (208 bytes, 52 words)
- [AES-256] 256-bit key (Nk = 8) needs to be expanded to (14+1)*128 = 1920 bits (240 bytes, 60 words)

Nk is the size of the unexpanded key in words (each word is 32 bits).

The start of the key schedule is the key as is (the unexpanded key that was given to the algorithm). Each round key being 16 bytes (4 words), this gives us the initial round key for AES-128, the initial round and half of the first round (1.5 round keys) for AES-192, and the initial and first round (2 round keys) for AES-256.

In the [FIPS 197](https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf) p19 algorithm, the key schedule is referred to as w, indexed with variable i in words (not bytes, so i*4 bytes), so the unexpanded key is placed in:
- AES-128: w[0..3]
- AES-192: w[0..5]
- AES-256: w[0..7]

(0-based with the upper bound included)

Then the algorithm has to fill in the remaining words:
- AES-128: w[4..43]
- AES-192: w[6..51]
- AES-256: w[8..59]

The key expansion algorithm performs three transformations every time the value of i is divisible by Nk (not including i=0, because it's already filled in):
- SubWord (which is [S-box substitution like in (B)](#b-s-box-substitution). I will use the same function we already defined, `subbytes`)
- RotWord
- XOR Rcon

> Every following word, w[i], is equal to the XOR of the previous word, w[i-1], and the word Nk positions earlier, w[i-Nk]. For words in positions that are a multiple of Nk, a transformation is applied to w[i-1] prior to the XOR, followed by an XOR with a round constant, Rcon[i]. This transformation consists of a cyclic shift of the bytes in a word (RotWord()), followed by the application of a table lookup to all four bytes of the word (SubWord()).  
—*[FIPS 197](https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf) p19*

For i % Nk != 0:

w[i] = w[i-1] XOR w[i-Nk]

For i % Nk == 0:

w[i] = SubWord(RotWord(w[i-1])) XOR Rcon[i] XOR w[i-Nk]

An example for AES-256, i=8:

w[8] = SubWord(RotWord(w[7])) XOR Rcon[8] XOR w[0]

There's one other thing that applies only in AES-256. Another transformation when i+4 is divisible by Nk (i.e. i % Nk = 4). It similar to before, but only SubWord(w[i-1]) XOR w[i-Nk]; no RotWord or Rcon.

> If Nk = 8 and i-4 is a multiple of Nk, then SubWord() is applied to w[i-1] prior to the XOR.  
—*[FIPS 197](https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf) p19*

For example, i=12 in AES-256:

w[12] = SubWord(w[11]) XOR w[4]

RotWord, which I'm going to call just rot, takes 4 bytes a₀ a₁ a₂ a₃ and reorders them such that the first byte is now last: a₁ a₂ a₃ a₀.
```javascript
function rot(s) {
  c = s.slice();  // copy
  for (let i = 0; i < 4; i++) {
    c[i] = s[(i+1)%4];
  }
  return c;
}
```
Since this function is so simple, it's often inlined (e.g. `[s[1], s[2], s[3], s[0]]` directly).

Rcon stands for round constant. It's 4 bytes (32 bits / a word) a₀ a₁ a₂ a₃ where a₁ to a₃ are always 0, and a₀ is 1 multiplied by x i-1 times in GF(2⁸), starting from i=1 and incrementing for each Rcon.

It is like 2^{i-1} (multiplying by x in GF(2⁸) is like multiplying by 2) up to i=8, because past that point we would exceed one byte, and that's what the GF(2⁸) shenanigans do, in essence; keep us under one byte.

This i is not the same i as before which we were using as the index for w. In fact it's i/Nk because we only XOR Rcon (and do the other transformations) when i is divisible by Nk. So to avoid confusion, instead of i I henceforth will be using j for the index to Rcon. It starts at 1 and increments each time the key expansion algorithm does the transformations (i.e. when i is divisible by Nk).

Algebraically, a₀ is evaluated as follows for each value of j:
```asciiart
 j=1 -> a₀ = 1
     -> 0b00000001 = 0x01
 j=2 -> 1x mod x⁸+x⁴+x³+x+1 = x
     -> 0b00000010 = 0x02
 j=3 -> 1x(x) mod x⁸+x⁴+x³+x+1 = x²
     -> 0b00000100 = 0x04
 ..
 j=8 -> x⁷ mod x⁸+x⁴+x³+x+1 = x⁷
     -> 0b10000000 = 0x80
 j=9 -> x⁸ mod x⁸+x⁴+x³+x+1 = x⁴+x³+x+1
     -> 0b00011011 = 0x1B
                                              1
              +--------------------------------
 x⁸+x⁴+x³+x+1 | x⁸+0x⁷+0x⁶+0x⁵+0x⁴+0x³+0x²+0x+0
                x⁸             +x⁴ +x³     +x+1
                -----------------------------
                                x⁴ +x³     +x+1
```
An easier way than the long division is to observe that in GF(2⁸), x⁸ ≡ x⁴+x³+x+1, and make this replacement. This is equivalent to a long division of x⁸ by the irreductible polynomial x⁸+x⁴+x³+x+1; we put 1 at the top (the quotient) then have to subtract 1(x⁸+x⁴+x³+x+1) from x⁸, which is XOR in this field so only cancels out the x⁸ term to give x⁴+x³+x+1.
```asciiart
 j=9 -> x⁸
 substituting in x⁸ ≡ x⁴+x³+x+1
 j=9 -> x⁴+x³+x+1
     -> 0b00011011 = 0x1B
 j=10 -> x⁹ = x(x⁸)
            = x(x⁴+x³+x+1)
            = x⁵+x⁴+x²+x
      -> 0b00110110 = 0x36
 j=11 -> x¹⁰ = x²(x⁸)
             = x²(x⁴+x³+x+1)
             = x⁶+x⁵+x³+x²
      -> 0b01101100 = 0x6C
 j=12 -> x¹¹ = x³(x⁸)
             = x³(x⁴+x³+x+1)
             = x⁷+x⁶+x⁴+x³
      -> 0b11011000 = 0xD8
```
Once we exceed j=12, if we do it like x^{i-1}, we have to substitute several times (x⁸ has to be substituted each time that it appears).
```asciiart
 j=13 -> x¹² = x⁴(x⁸)               [substitute x⁸]
             = x⁴(x⁴+x³+x+1)
             = x⁸+x⁷+x⁵+x⁴          [substitute x⁸ again]
             = (x⁴+x³+x+1)+x⁷+x⁵+x⁴
             = x⁷+x⁵+x³+x+1         [x⁴ went away because it's an XOR]
      -> 0b10101011 = 0xAB
```
If you know the result for j=12, you can multiply that by x and you will only have to substitute once:
```asciiart
 j=13 -> x(x⁷+x⁶+x⁴+x³) = x⁸+x⁷+x⁵+x⁴  [substitute x⁸]
                        = (x⁴+x³+x+1)+x⁷+x⁵+x⁴
                        = x⁷+x⁵+x³+x+1
      -> 0b10101011 = 0xAB
```

{% include note.html content='
> [!NOTE]
> This algebraic manipulation is technically in GF(2)[x]/(x⁸+x⁴+x³+x+1) rather than in GF(2⁸)?
> GF(2⁸)=GF(2)[x]/(x⁸+x⁴+x³+x+1)?
' %}

In practice, j never gets this high. The maximum value it takes is 10 for AES-128, 8 for AES-192, and 7 for AES-256, because we only need to fill in w[4..43], w[6..51], or w[8..59] respectively and we only do the transformations when i is divisible by Nk (4, 6, or 8 respectively);
- AES-128: i = 4, 8, 12, 16, 20, 24, 28, 32, 36, 40 (j = 1, 2, ..., 10)
- AES-192: i = 6, 12, 18, 24, 30, 36, 42, 48 (j = 1, 2, ..., 8)
- AES-256: i = 8, 16, 24, 32, 40, 48, 56 (j = 1, 2, ..., 7)

Bitwise, multiplying by x in GF(2⁸) is done using the following algorithm in C (I am intentionally giving it first in C, in JavaScript there is an additional complication):
```c
uint8_t xtime(uint8_t a) {
    if (a & 0x80) {
        return (a << 1) ^ 0x1B;
    } else {
        return a << 1;
    }
}
```
- `a` is an 8-bit unsigned integer; anything above 8 bits gets truncated, so the degree of the polynomial representation of `a` is ≤ 7 (we will never get a term higher than x⁷).
- `a << 1` does the multiplication (bitshifting to the left is like multiplying by 2, which is like multiplying by x).
- Testing `a & 0x80` tells us when the highest bit is on (0x80 = 0b10000000) and thus if the degree of the corresponding polynomial is 7 (i.e. there's a x⁷ term), meaning that if we multiply by x again we'll get a x⁸ term.
- `^ 0x1B` does the reduction. 0x1B = 0b00011011 = x⁴+x³+x+1. It's like "subtracting" (XOR) by x⁴+x³+x+1 in the algebraic representation when we do the long division of a polynomial of degree 8 by the irreductible polynomial x⁸+x⁴+x³+x+1, after we write 1 at the top in the quotient.

This is what I mean:
```asciiart
                                 1
             +--------------------
x⁸+x⁴+x³+x+1 | x⁸+ ..lower terms..
               x⁸+x⁴+x³+x       +1
```
(after this point you "subtract" [XOR] x⁸+x⁴+x³+x+1 from the polynomial you are dividing)

With a ternary conditional operator we can write it shorter:
```c
uint8_t xtime(uint8_t a) {
    return (a << 1) ^ ((a & 0x80) ? 0x1B : 0);
}
```

In JavaScript we can't limit the size of `a` to 8 bits, so we have to truncate it ourselves, which we can do by `& 0xFF` the result. It's like a mask that's keeping in only the lowest byte.
```javascript
function xtime(a) {
  return (
    ((a << 1) ^ ((a & 0x80) ? 0x1B : 0))
    & 0xFF
  );
}
```

Using this to obtain the Rcon for a given value of j:
```javascript
// dependencies: xtime
function rcon(j) {
  rc = new Uint8Array(4);
  rc[0] = 1;
  for (let i = 1; i < j; i++) {
    rc[0] = xtime(rc[0]);
  }
  return rc;
}
```
This is sometimes implemented instead as a table (in that case, get index j-1, since j is 1-based).
```javascript
const rcon = [1, 2, 4, 8, 16, 32, 64, 128, 27, 54];
```
Since all of these values are in line with powers of two other than the last two, another way is to start with 1 and leftshift each time (`<<= 1`), resetting to 27 (0x1b) the 9th time.

Yet another way, [that I saw Sam Trenholme do](https://www.samiam.org/key-schedule.html), is to use the Galois multiplication function we had to implement for [TODO I can't remember, B or D?], start with the value 1 and repeatedly multiply it by 2 with it (which handles the reduction).

We now have all the elements we need for the complete key expansion algorithm. Algorithm given in [FIPS 197](https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf) p19 translated to JavaScript and substituting in the functions we defined above (subbytes, rot, rcon):
```javascript
// dependencies: wordFromBytes, bytesFromWord, xorBytes, subbytes, rot, rcon
function expandkey(key, w, Nk) {
  i = 0;

  while (i < Nk) {
    w[i] = wordFromBytes(key, 4*i);  // 4*i to 4*i+3
    i += 1;
  }

  i = Nk;

  while (i < w.length) {
    temp = bytesFromWord(w[i-1]);
    if (i % Nk === 0) {
      temp = xorBytes(subbytes(rot(temp)), rcon(i/Nk));
    } else if (Nk > 6 && i % Nk === 4) {  // AES-256
      temp = subbytes(temp);
    }
    w[i] = w[i-Nk] ^ wordFromBytes(temp);
    i += 1;
  }
}
```
Where `wordFromBytes` is a utility function to help with the conversion from `key` members, which are in bytes, to `w` members, which are in words (4 bytes):
```javascript
function wordFromBytes(bytes, offset=0) {
  return (
    (bytes[offset]     << 24) |
    (bytes[offset + 1] << 16) |
    (bytes[offset + 2] <<  8) |
    (bytes[offset + 3])
  ) >>> 0;
}
```
The `>>> 0` is to force an unsigned 32-bit integer, as otherwise we could get a negative number (e.g. 0x80 << 24 = -2147483648, 0x80 << 24 >>> 0 = 2147483648) (cf [MDN: Unsigned left shift](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unsigned_right_shift). The French version of the article says that this operator, unlike the other binary operators, returns an unsigned 32-bit integer. The English version for some reason doesn't say that, but hopefully it's true. Maybe it's inaccurate because it just fills the sign bit?)

And `bytesFromWord` is a function that does the opposite, because `rot` and `subbytes` need to operate on bytes, and expect an array (although I could have, and maybe should have, made them able to operate on a number).
```javascript
function bytesFromWord(word) {
  return new Uint8Array([
    (word >> 24) & 0xFF,
    (word >> 16) & 0xFF,
    (word >> 8) & 0xFF,
    word & 0xFF
  ]);
}
```

(We will rewrite the function to use bytes and not need these functions. I wanted to stay close to the FIPS 197 algorithm for the first go)

And finally, we can't XOR Uint8Arrays directly (JavaScript will silently fail and return 0), so we also need this:
```javascript
function xorBytes(a, b) {
  return new Uint8Array(
    a.map((x, i) => x ^ b[i])
  );
}
```

[test: `w = new Uint32Array(44); expandkey([00,01,02,03,04,05,06,07,08,09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f], w, 4)`]
[`Array.from(w).map(x => x.toString(16)).join()`]

`w` is the output, meant to be length `(numrounds+1)*128` bits, as covered before, but we don't need this as an argument as we can just return it, and we don't need `Nk` as an argument either since we can just work it out from the length of the key.
```javascript
// dependencies: wordFromBytes, bytesFromWord, xorBytes, subbytes, rot, rcon
function expandkey(key) {
  i = 0;
  Nk = key.length / 4;
  b = key.length;
  // rounds (10 / 12 / 14)
  Nr = b > 16 ? (b > 24 ? 14 : 12) : 10;
  w = new Uint32Array(4 * (Nr + 1));
  while (i < Nk) {
    w[i] = wordFromBytes(key, 4*i);  // 4*i to 4*i+3
    i += 1;
  }
  i = Nk;
  while (i < w.length) {
    temp = bytesFromWord(w[i-1]);
    if (i % Nk === 0) {
      temp = xorBytes(subbytes(rot(temp)), rcon(i/Nk));
    } else if (Nk > 6 && i % Nk === 4) {  // AES-256
      temp = subbytes(temp);
    }
    w[i] = w[i-Nk] ^ wordFromBytes(temp);
    i += 1;
  }
  return w;
}
```

[test: `res = expandkey([00,01,02,03,04,05,06,07,08,09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f])`]
[`Array.from(res).map(x => x.toString(16)).join()`]

Instead of working in words, we can work directly in bytes.
- Store the schedule in a `Uint8Array` instead of a `Uint32Array`. It no longer makes sense to call the variable `w`, but I guess we'll keep calling it `w`.
- Instead of `4 * (Nr + 1)` members it needs to have `16 * (Nr + 1)` members (×4)
- Loop every 4 bytes
- `i` is now our index in bytes, so after setting the key start at `key.length` and stop before `w.length`, incrementing by 4 each iteration
- Transformations on a subarray of 4 bytes
- The equivalent of `i % Nk` from when i and Nk were in words, is `i % key.length` with both i and key in bytes. Still `=== 0`. For the special AES-256 case, which was `i % Nk === 4`, it is now ×4: `i % key.length === 16`.
- Similarly, the argument for Rcon will now be `i/key.length` instead of `i/Nk`.

```javascript
// dependencies: xorBytes, subbytes, rot, rcon
function expandkey(key) {
  b = key.length;
  // rounds (10 / 12 / 14)
  Nr = b > 16 ? (b > 24 ? 14 : 12) : 10;
  const w = new Uint8Array(16 * (Nr + 1));
  w.set(key);
  for (let i = b; i < w.length; i += 4) {
    let temp = w.slice(i-4, i);
    if (i % b === 0) {
      temp = xorBytes(subbytes(rot(temp)), rcon(i/b));
    } else if (Nr > 12 && i % b === 16) {  // AES-256
      temp = subbytes(temp);
    }
    // Apply to the 4 bytes in w
    for (let j = 0; j < 4; j++) w[i+j] = w[i+j-b] ^ temp[j];
  }
  return w;
}
```

We can further reduce the dependencies to just `subbytes` and `rcon` by doing `rot` inline. For `rcon`, only the first byte is non-zero, so we can do simply `temp[0] ^= rcon(i/b)[0];` to XOR just the first byte.
```javascript
// dependencies: subbytes, rcon
function expandkey(key) {
  b = key.length;
  // rounds (10 / 12 / 14)
  Nr = b > 16 ? (b > 24 ? 14 : 12) : 10;
  const w = new Uint8Array(16 * (Nr + 1));
  w.set(key);
  for (let i = b; i < w.length; i += 4) {
    let temp = w.slice(i-4, i);
    if (i % b === 0) {
      temp = subbytes([temp[1], temp[2], temp[3], temp[0]]);
      temp[0] ^= rcon(i/b)[0];
    } else if (Nr > 12 && i % b === 16) {  // AES-256
      temp = subbytes(temp);
    }
    // Apply to the 4 bytes in w
    for (let j = 0; j < 4; j++) w[i+j] = w[i+j-b] ^ temp[j];
  }
  return w;
}
```

[test: `res = expandkey([00,01,02,03,04,05,06,07,08,09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f])`]
[`Array.from(res).map(x => x.toString(16).padStart(2, '0')).join()`]

The comments in [Moserware's `KeySchedule.cs`](https://github.com/moserware/AES-Illustrated/blob/master/KeySchedule.cs) describe the transformation visually.

> you take the last column of the previous round key and move it up by one byte (the previous top byte goes to the bottom). Then you put each of the bytes of that column through the S-box. Then you XOR the new top byte with the round key. Finally, you xor the whole column with the column Nb columns earlier. The other columns are made by xor-ing the previous column with the column Nb columns earlier.

I guess you can look at a sequence of four bytes as a column, if we're saying:
```asciiart
 | s0  s4  s8  s12 |
 | s1  s5  s9  s13 |
 | s2  s6  s10 s14 |
 | s3  s7  s11 s15 |
```
which is like:
```asciiart
 | w0 w1 w2 w3 |
```
each word being a column.

I was confused why in his example he has 4 empty elements. I guess it's just ASCII spaces.
```asciiart
 | S |   |   |   |
 | O | 1 | B | K |
 | M | 2 | I | E |
 | E | 8 | T | Y |
```

In hex (and w indices added to give us an idea of where we are):
```asciiart
  w[0] w[1] w[2] w[3]
 | 53 | 20 | 20 | 20 |
 | 4F | 31 | 42 | 4B |
 | 4D | 32 | 49 | 45 |
 | 45 | 38 | 54 | 59 |
```

{% include note.html content='
> [!NOTE]
> For him in the last column the K turns into 53, and I\'m not sure why. The ASCII character code of K is 0x4B (or 75 decimal). For the rest he did write their character codes: E 0x45, Y 0x59, and space 0x20. 0x53 is S. Later putting it through the S-box he gets 0xB3; 0x53 through the S-box gives 0xED, 0x4B gives 0xB3, so he probably did use 0x4B for the first element and just wrote the wrong thing there.
' %}

(1) Take the last column:
```asciiart
 | 20 |             |   |
 | 4B |    (i.e.:)  | K |
 | 45 |             | E |
 | 59 |             | Y |
```
(2) RotWord:
```asciiart
 | 4B |             | K |
 | 45 |    (i.e.:)  | E |
 | 59 |             | Y |
 | 20 |             |   |
```
(3) Put each byte through the S-box:
```asciiart
 | B3 |
 | 6E |
 | CB |
 | B7 |
```
(4) XOR with the first Rcon (which only changes the first element):
```asciiart
 | B3 | XOR | 1 | = | B2 |
 | 6E | XOR | 0 | = | 6E |
 | CB | XOR | 0 | = | CB |
 | B7 | XOR | 0 | = | B7 |
```
He says this step is to prevent symmetry.

(5) XOR with the column 4 columns ago, w[0]:
```asciiart
 | B2 | XOR | 53 | = | E1 |
 | 6E | XOR | 4F | = | 21 |
 | CB | XOR | 4D | = | 86 |
 | B7 | XOR | 45 | = | F2 |
```

We've now got:
```asciiart
  w[0] w[1] w[2] w[3] w[4]
 | 53 | 20 | 20 | 20 | E1 |
 | 4F | 31 | 42 | 4B | 21 |
 | 4D | 32 | 49 | 45 | 86 |
 | 45 | 38 | 54 | 59 | F2 |
```

[test: `res = expandkey([0x53, 0x4F, 0x4D, 0x45, 0x20, 0x31, 0x32, 0x38, 0x20, 0x42, 0x49, 0x54, 0x20, 0x4B, 0x45, 0x59])`]
[`Array.from(res).map(x => x.toString(16).padStart(2, '0')).join()`]

For getting the next column and each subsequent one until the next multiple of 4 (where we have to do the transformations again), we just do 1 and 5: take the last column and XOR with the column 4 columns ago.

w[6] will be w[5] XOR w[1], w[7] will be w[6] XOR w[2]. For w[8] you have to put w[7] through the transformations again (this time with the second Rcon), then XOR with w[4].

So the steps 2-4 transformations are only done when "starting a new round key", i.e. for the first word (column) of a round key.

This is the algorithm for AES-128. For AES-192 and AES-256, the size of each round key is the same, but since we start with 6 (AES-192) and 8 (AES-256) words/columns from the initial key instead of 4, each "batch" of columns is that size (6 or 8 instead of 4), and we do the transformations at the start of each batch, not at the start of each round key.

For AES-256 there is also the extra subbytes when `i % Nk = 4`, which means the 4th column of each generated batch, so it's the first column of the second round key of each generated batch (since each generated batch in AES-256 is 2 round keys). The first one will be at i=12.
```asciiart
 round |
 key # | words (columns)
 ------+----------------
   1   | w0 w1 w2 w3       \
   2   | w4 w5 w6 w7       / ↖ initial key
 ------+----------------
   3   | [w8] w9 w10 w11   \
   4   | (w12) w13 w14 w15 / ↖ first generated batch
 ------+----------------
   5   | [w16] w17 w18 w19 \
   6   | (w20) w21 w22 w23 / ↖ second generated batch
       ...

         [] marks i % Nk = 0 (where i>Nk)
         () marks i % Nk = 4 (where i>Nk)
```

[FIPS 197](https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf) appendix (p35) and [Trenholme's key schedule page](https://www.samiam.org/key-schedule.html) have test vectors.

## cont (B)
[There is an algorithm for computing the S-box with an explanation by fgrieu, 2022](https://crypto.stackexchange.com/questions/98396/how-does-this-code-calculating-aes-s-box-work), but I am not able to follow, I don't understand the notation. I guess instead of just throwing my hands, I will try to at least list what I don't understand.

The algorithm itself is from [merricx.github.io/aes-sbox](https://merricx.github.io/aes-sbox/). It also allows you to put in a different polynomial, defaulting to `1 + x + x**3 + x**4 + x**8`.

The first thing I don't understand is the statement in the first paragraph that the code tabulates Qᵢ = Gⁱ mod P. What does that mean?
- G is the generating polynomial G=1+x; [Moserware] explains this in act 4 scene 10, if you keep multiplying it by itself, you generate all possible polynomials below x⁸.
- P is the irreductible polynomial x⁸+x⁴+x³+x+1 (he writes it in the reverse order for some reason, with the least important terms first, but it's equivalent).
- Q ?

It is followed by this statement:

> The multiplicative inverse of R=Qᵢ is Q₂₅₅₋ᵢ.

Possibly Qᵢ is just defined as Gⁱ mod P. Which is just the generating polynomial taken to some power i, reduced with the irreductible polynomial to avoid exceeding x⁷.

So to get the multiplicative inverse of a given number, if you know its (x+1)ⁱ representation you get the multiplicative inverse from (x+1)²⁵⁵⁻ⁱ? (mod P in both cases, needless to say)

The first bit of the code, slightly simplified to always use the default irreductible polynomial (also, it's spelled as "irreducible", has it been "irreducible" all along and I've been spelling it wrong???):
```javascript
// 1
irreducible_poly = "1 + x + x**3 + x**4 + x**8";
p = parseInt(eval(irreducible_poly.replace(/x/g, '10')), 2);
// 2
let t = new Uint32Array(256);
for (let i = 0, x = 1; i < 256; i++) {
  t[i] = x;
  x ^= (x << 1) ^ ((x >>> 7) * p);
}
```
`p` evaluates to 283 (0x11b)

`t` evaluates to [1,3,5,15,17,51,85,255,26,46,114,150,161,248,19,53,95,225,56,72,216,115,149,164,247,2,6,10,30,34,102,170,229,52,92,228,55,89,235,38,106,190,217,112,144,171,230,49,83,245,4,12,20,60,68,204,79,209,104,184,211,110,178,205,76,212,103,169,224,59,77,215,98,166,241,8,24,40,120,136,131,158,185,208,107,189,220,127,129,152,179,206,73,219,118,154,181,196,87,249,16,48,80,240,11,29,39,105,187,214,97,163,254,25,43,125,135,146,173,236,47,113,147,174,233,32,96,160,251,22,58,78,210,109,183,194,93,231,50,86,250,21,63,65,195,94,226,61,71,201,64,192,91,237,44,116,156,191,218,117,159,186,213,100,172,239,42,126,130,157,188,223,122,142,137,128,155,182,193,88,232,35,101,175,234,37,111,177,200,67,197,84,252,31,33,99,165,244,7,9,27,45,119,153,176,203,70,202,69,207,74,222,121,139,134,145,168,227,62,66,198,81,243,14,18,54,90,238,41,123,141,140,143,138,133,148,167,242,13,23,57,75,221,124,132,151,162,253,28,36,108,180,199,82,246,1]

> It computes table t[i] representing the binary polynomial Qᵢ = Gⁱ mod P

> That Qᵢ is computed per the recurrence Qᵢ₊₁ = Qᵢ(1+x) mod P with Q₀=1, translating¹ to t[i+1] = t[i] ^ ((t[i] << 1) ^ ((t[i] >>> 7) * p)) with t[0] = 1 under said convention.

The "convention", as far as I can understand, is representing bytes as polynomials with x=2, like x⁸+x⁴+x³+x+1 representing 0b100011011 = 0x11b (which is more than one byte, but you get what I'm saying).

Does it mean `t` has all polynomials Qᵢ for i 0 to 255? It seems to be, because they follow it with examples:
- Q₀ is the binary polynomial 1, represented by t[0] = 1
- Q₁ = 1+x, represented by t[1] = 3
- Q₂ = (1+x)² = 1+x², represented by t[2] = 5
- Q₇ = (1+x)⁷ = 1+x+x²+x³+x⁴+x⁵+x⁶+x⁷, represented by t[7] = 0xff = 255
  + They actually wrote t[2], but it's probably an error.
- Q₈ = (1+x)⁸ mod P = (1+x⁸) mod P = x+x³+x⁴, represented by t[8] = 0x1a = 26.
  + How does (1+x)⁸ turn to (1+x⁸) ?

There is little footnote at the end with justification for how the bitwise operations correspond to the algebra. He sets it out as a series of steps, until reaching the full `t[i] ^ ((t[i] << 1) ^ ((t[i] >>> 7) * p))`:

- `t[i]` is an integer in [0,2⁸) and represents Qᵢ of degree <8
  + This notation means 0 to 2⁸, not including 2⁸. 0-255. Due to the loop being: `for (let i = 0, x = 1; i < 256; i++)`
  + Qᵢ = (x+1)ⁱ mod P
  + I don't really get why it represents Qᵢ. I guess because x+1 (mod P), the generating polynomial, is capable of generating all bytes? (their polynomial representations)
- `t[i] << 1` is an even integer in [0,2⁹) and represents Qᵢx.
  + because it's `t[i]` leftshifted, which is like multiplying by 2
  + Qᵢx = x(1+x)ⁱ presumably?
  + Multiplying by 2 is like multiplying by x
- `t[i] >>> 7` is either 0 or 1. It's the coefficient of the term of order x⁷ in Qᵢ.
  + `t[i]` rightshifted by 7. Conventionally, `>>>` is used instead of `>>` in JavaScript, because the latter can result in negative numbers for certain values. In this case I think it doesn't matter, because `t[i]` is just 1 byte (8 bits), and JavaScript uses 32-bit integers.
  + It's the coefficient of x⁷ because it's the 8th byte. Like: 0b10000000 = x⁷ (or if it's 0 then it's simply not present. It's 1, present, or 0, not present.)
- `(t[i] >>> 7) * p` is correspondingly either 0 or 0x11b = 283, representing 0 or P.
  + because it's multiplying `p` by either 0 or 1.
- `(t[i] << 1) ^ ((t[i] >>> 7) * p)` correspondingly represents (Qᵢx) or (Qᵢx)+P, and (by examination of the two cases) is an integer in [0,2⁸), thus represents a binary polynomial of degree <8, which thus is (Qᵢx) mod P.
  + This is XORing `t[i] << 1`, which we said represents x(x+1)ⁱ, with `((t[i] >>> 7) * p)`, which we said represents either 0 or the irreducible polynomial, depending on whether there is an x⁷ term present. In [the section on key expansion](#aes-key-expansion) we learned that XORing a polynomial of degree 8 by the irreducible polynomial is a shortcut to long division (which we do to apply mod P, i.e. reduce the terms that are x⁸ or above), but here it's x⁷, so I'm not sure why we XOR by P if x⁷ is present. Ah, it's because it gets multiplied by another x; `t[i]` is (x+1)ⁱ, but `t[i] << 1` is x(x+1)ⁱ. In `((t[i] >>> 7) * p)` it's checking for the presence of x⁷ in `t[i]`, so if it's there, multiplying `t[i]` by another x means there will be x⁸.
- `t[i] ^ ((t[i] << 1) ^ ((t[i] >>> 7) * p))` is thus an integer in [0,28) and represents Qᵢ+(Qᵢx) mod P = Qᵢ(1+x) mod P = Qᵢ₊₁, of degree <8.
  + Qᵢ is (x+1)ⁱ mod P, so Qᵢ(1+x) is (x+1)ⁱ(1+x) = (x+1)ⁱ⁺¹ mod P

> In C, the standard likely-constant-time idiom for this expression would be (essentially with & - used instead of multiplication):  
`t[i+1] = t[i] ^ ((t[i] << 1) ^ (p & -(t[i] >> 7)))`.

What we needed for the multiplicative inverse was (x+1)²⁵⁵⁻ⁱ. Is this were we're going? I don't really see how.

Actually, if you remember from before the justification, this whole bit is just to say the table generated in `t` is now all representations of Qᵢ, for i 0 to 255. It starts from the known Q₀ = 1, and computes Q₁₊₁ each iteration until we have all of Q₀ (in `t[0]`) to Q₂₅₅ (in `t[255]`).

So in order to find the multiplicative inverse, do we have to find the number in `t`, then say it's in `t[5]`, the multiplicative inverse will be in `t[255-5]` = `t[250]`? `t[5]` is 51 (0x33), `t[250]` is 108 (0x6c). Looking at [public.hochschule-trier.de/~knorr/AES/index.php?xy=33](https://public.hochschule-trier.de/~knorr/AES/index.php?xy=33), the multiplicative inverse of 0x33 is indeed 0x6c.

But... it relies on a table anyway. And even if there was a way to know the i we need without the whole table, it would not be constant time anyway. So how are constant-time implementations doing it? And what's the relation of all this to the extended euclidean algorithm?

I'm suddenly remembering having seen [this March 2026 Reddit thread](https://www.reddit.com/r/cryptography/comments/1rpam8b/) where a few people argue about whether or not the extended euclidean algorithm is "used in AES". Cryptizard claimed "it is not included in any implementation of the AES algorithm". I thought he just meant because most people use tables for the S-box.

For completion, let's continue going through merricx's algorithm and fgrieu's explanation thereof.
```javascript
let Sbox = new Uint32Array(256);
Sbox[0] = 0x63;
for (let i = 0; i < 255; i++) {
  let x = t[255-i];
  x |= x << 8;
  x ^= (x >> 4) ^ (x >> 5) ^ (x >> 6) ^ (x >> 7);
  Sbox[t[i]] = (x ^ 0x63) & 0xFF;
}
```

> in the loop `i` goes from 0 to 254 that yields the multiplicative inverse of each of the 255 non-zero polynomials of degree <8. The loop then merely applies² the affine transformation specified in the rest of the [definition of the AES S-box](https://csrc.nist.gov/csrc/media/projects/cryptographic-standards-and-guidelines/documents/aes-development/rijndael-ammended.pdf#page=13). The zero polynomial is special-cased.

> For example: when in the loop `i` = 8, `t[i]` is `t[8]` = 0x1a = 26 representing Q₈=x+x³+x⁴, and `t[255-i]` (going to `x`, unrelated to x [i.e. to the variable he named x for whatever reason, which is not the same as the x in the polynomial representation]) is `t[247]` = 0xfd = 253 representing the polynomial Q₂₄₇=1+x²+x³+x⁴+x⁵+x⁶+x⁷. That Q₂₄₇ is the multiplicative inverse of Q₈. By the definition of the AES S-box, there only remains to apply the affine transformation² to x, then set the result in `Sbox[t[i]]` = Sbox[26].

Footnote 2:

> The statement `x |= x << 8;` duplicates the bits in variable `x` (representing the modular inverse of Qᵢ) so that subsequent right shifts become equivalent to rotation when it comes to the low-order 8 bits. The statement `x ^= (x >> 4) ^ (x >> 5) ^ (x >> 6) ^ (x >> 7);` implements the [circulant matrix](https://en.wikipedia.org/wiki/Circulant_matrix) multiplication. Then `^ 0x63` (representing polynomial 1+x+x⁵+x⁶) is the constant addition, and `& 0xFF` keeps the low-order 8 bits (terms of degree <8), undoing the duplication.

I don't want to go further into the affine transformation before working out a good way to find the multiplicative inverse.

Maybe go back to the algebra and try to work out a naïve algorithm ourselves with gmul and reduction like in expandkey? We also need division. And we actually don't need reduction, or at least I didn't seem to ever need to do mod P while applying the algorithm, I don't think you're supposed to reduce it in that algorithm.

> Dividing a by b in Rijndael's galois field is performed by taking the logarithm of a and subtracting the logarithm of b from it, modulo 255.  
-[*Trenholme*](https://www.samiam.org/galois.html)

This is not good news. Log is also done via a table.

It is maybe untrue that you need logarithms for division. In [this 2016 video by Creel](https://www.youtube.com/watch?v=dRYHSf5A4lw) 15 minutes in, he demonstrates how you can do it by aligning the highest term and XORing. This is similar to what we were doing for Rcon with x⁸, generalised for any degree polynomial. But actually that gives only the remainder, and what we're interested in in the extended euclidean algorithm is the quotient.

Can you maybe get the quotient from the remainder?

A long division from earlier:
```asciiart
             x⁴     +x²
    +----------------------
 x² | x⁶+0x⁵+x⁴+0x³+0x²+x+1
      x⁶
      --
             x⁴        +x+1
             x⁴
             --
                        x+1
```
x⁶+x⁴+x+1 div x²
1. Put x⁴ in the quotient to cancel the x⁶ (6-2 = 4)
2. x⁶+x⁴+x+1 XOR x²(x⁴) = x⁴+x+1
3. Put x² in the quotient to cancel the x⁴ (4-2 = 2)
4. x⁴+x+1 XOR x²(x²) = x+1

I guess we can check the degree by checking whether a given bit is on or off, like the `>>> 7` we saw earlier to check if there's an x⁷, then work out the first term of the quotient based on that. To check lower bits, `>>> i & 1`; the `>>> i` moves the bit down to the lowest bit, and the `& 1` keeps only that bit, getting rid of any higher ones.

a div b
1. Put x^{deg(a)-deg(b)} in the quotient
2. a XOR b(x^{deg(a)-deg(b)}) [remainder]
3. Put x^{deg(a XOR b(x^{deg(a)-deg(b)}))-deg(b)} in the quotient
4. (a XOR b(x^{deg(a)-deg(b)})) XOR b(x^{deg(a XOR b(x^{deg(a)-deg(b)}))-deg(b)})
5. When to stop?

In the previous example, we stopped when the remainder was x+1, because deg(x+1) < deg(x²). So I guess when doing deg(a)-deg(b) you have to check it's not negative before deciding to put x^{deg(a)-deg(b)} in the quotient.

```javascript
function deg(x) {
  let r = -1;
  for (let i = 0; i < 8; i++) {
    let bit = (x >>> i) & 1;
    r = bit ? i : r;
  }
  return r;
}
```
- No early return, in order for it to be constant time.
- deg(0) must give -1 to avoid infinite loop while using it to divide while degree difference ≥ 0

Multiplying by x is like multiplying by 2, so the bitwise equivalent for taking x to the power of deg(a)-deg(b) is leftshifting by that amount.
But leftshifting what? 1?

Addition is XOR so to put something in the quotient I think we need to XOR into it.

a div b
- d = deg(a)-deg(b)
- while d ≥ 0
  + newterm = xᵈ
  + add newterm to quotient (XOR)
  + subtract b×newterm from remainder (XOR b << newterm)
  + d = deg(remainder)-deg(b)

Or:
- d = deg(a)-deg(b)
- while d ≥ 0
  + newterm = 1 << d
    - (xᵈ)
  + quotient ^= newterm
  + remainder ^= b << newterm
    - (subtract b(xᵈ))
  + d = deg(remainder)-deg(b)

{% include note.html content='
> [!NOTE]
> It\'s wrong to do `remainder ^= b << newterm` to subtract b(xᵈ), as I find out later.
' %}

`remainder` needs to start at `a` and get progressively subtracted from. `quotient` needs to start at 0 and get added to.

```javascript
function gdiv(a, b) {
  let quotient = 0;
  let remainder = a;
  let d = deg(remainder)-deg(b);
  while (d >= 0) {
    let newterm = 1 << d;
    quotient ^= newterm;
    remainder ^= b << newterm;
    d = deg(remainder)-deg(b);
  }
  return quotient;
}
```
Not constant time. And can you end up with an infinite loop?

gdiv(0,0)
- quotient = 0
- remainder = 0
- d = deg(0)-deg(0) = -1--1 = 0
- newterm = 1 << d = 1 << 0 = 1
- quotient = quotient ^ newterm = 0 ^ 1 = 1
- remainder = remainder ^ (b << newterm) = 0 ^ (0 << 1) = 0
- d = deg(remainder)-deg(b) = deg(0)-deg(0) = -1--1 = 0
- continues looping forever. although quotient changed, it doesn't affect d.

Have to forbid dividing by 0.

gdiv(1,1)
- quotient = 0
- remainder = 1
- d = deg(1)-deg(1) = 0-0 = 0
- newterm = 1 << d = 1 << 0 = 1
- quotient = quotient ^ newterm = 0 ^ 1 = 1
- remainder = remainder ^ (b << newterm) = 1 ^ (1 << 1) = 3
- d = deg(remainder)-deg(b) = deg(1)-deg(1) = -1--1 = 0
- continues looping forever.

The polynomial representations are 1 and 1. The quotient should indeed be 1, but the remainder should be 1×1 = 1, not 3. Doing `remainder ^ (b << newterm)` is not correct then to subtract b(xᵈ)?

It's really confusing.

newterm is 1 << d

b << 1 << d ≠ b << d

1 << 1 << 0 ≠ 1 << 0

2 ≠ 1

To represent xᵈ = 1(xᵈ), we leftshift 1 d times: 1 << d

To represent b(xᵈ), leftshift b d times: b << d

Leftshifting is not multiplication, it's specifically *multiplication by x*.

[Shifting is kind of like how in decimal when multiplying by 10 we move the decimal point. 1.2×10 = 12. If we were to multiply by 10², we move it 2 places: 120. In this analogy, the equivalent of b << 1 << d is like evaluating separately 10² by moving the decimal point 2 places to get 100, then shifting the decimal point in 1.2 100 places, which is obviously not correct.]

```javascript
function gdiv(a, b) {
  if (b == 0) throw new RangeError("b must not be 0 (can't divide by 0)");
  let quotient = 0;
  let remainder = a;
  let d = deg(remainder)-deg(b);
  while (d >= 0) {
    quotient ^= 1 << d;   // xᵈ
    remainder ^= b << d;  // b(xᵈ)
    d = deg(remainder)-deg(b);
  }
  return quotient;
}
```

That's one step towards being able to do the extended euclidean algorithm. We also need multiplication, and not just by x, so bitshifting is not sufficient. Things like (x²+1)(x⁶+x⁴+x+1).

That old beast, gmul. I'm scared of it. But why is it so complicated? In theory it should be doing the same as polynomial multiplication. Maybe we could work something out naïvely like we did for division. Let's expand the terms and consider the problems involved in doing something like this bitwise.

TODO

[wait, that's not actually that complicated, kind of similar to xtime]
```javascript
// multiplication GF(2⁸)
const gmul = (a, b) => {
  let r = 0;
  while (b) {
    if (b & 1) r ^= a;
    // multiply by 2 preventing overflow
    // if a&0x80 -> exceeds 8 bits (1000 0000)
    a = ((a << 1) ^ (a & 0x80 ? 0x1b : 0)) & 0xff;
    b >>= 1;
  }
  return r;
};
```

- @conf crypt : cont AES mixcols manuel
    + t@conf crypt : AES gmul
    + t@conf crypt : AES euclide étendu bitwise
    + t@conf crypt : AES subbytes chercher autre impl
    + t@conf crypt : AES keyexpansion annexe fips
    + t@conf crypt : AES variantes constant-time des fonctions
    + t@conf crypt : AES algèbre quel GF
    + t@conf crypt : vérification que GCM compresse tjrs l'iv à 12 bytes
    + t@conf crypt : section W1
    + t@conf crypt : section W2
    + t@conf crypt : section W3 partie 2
    + t@conf crypt
    + t@conf crypt : démo CTR vuln 1
    + t@conf crypt : démo CTR vuln 2
    + t@conf crypt : RSA-OAEP
    + t@conf crypt : AES-CBC
    + t@conf crypt : AES-GCM

tabs
- https://developer.mozilla.org/en-US/docs/Web/API/AesGcmParams
- https://github.com/mdn/dom-examples/blob/main/web-crypto/encrypt-decrypt/rsa-oaep.js
- https://theswissbay.ch/pdf/Whitepaper/Crypto/GCM%2C%20GHASH%20and%20weak%20keys%20-%20Markku-Juhani%20O.%20Saarinen.pdf
- https://dev.to/moritzhoeppner/how-to-implement-ghash-3c0h
- https://www.seald.io/fr/blog/3-erreurs-courantes-quand-on-implemente-du-chiffrement W1, W2, nuances GCM, attaques Math.random
- https://asecuritysite.com/blogs/chapter04 W1
- [Wikipédia: Mode d'opération (cryptographie)](https://fr.wikipedia.org/wiki/Mode_d%27op%C3%A9ration_(cryptographie))
- https://asecuritysite.com/subjects/chapter88 aes encryption
- [synthèse aes128 pdf](https://www.emse.fr/~dutertre/documents/synth_AES128.pdf)
- [xilink.github aes](https://xilinx.github.io/Vitis_Libraries/security/2019.2/guide_L1/internals/iaes.html)
- https://engineering.purdue.edu/kak/compsec/NewLectures/Lecture8.pdf
- https://github.com/moserware/AES-Illustrated/blob/master/FiniteFieldMath.cs
- https://raw.githubusercontent.com/moserware/AES-Illustrated/refs/heads/master/Layers/MixColumns.cs
- https://raw.githubusercontent.com/moserware/AES-Illustrated/refs/heads/master/SubstitutionBox.cs
- https://raw.githubusercontent.com/moserware/AES-Illustrated/refs/heads/master/FiniteFieldMath.cs
- https://www.samiam.org/mix-column.html
- [polfosol](https://github.com/polfosol/micro-AES/blob/master/micro_aes.c)
- [donggeunkwon](https://github.com/donggeunkwon/aes/blob/master/aes/core/_core.py)
- [marek](https://github.com/MarekKnapek/mk_crypto/blob/mkcryptc/src/mk_aes.c)
- https://github.com/Braincoke/cryptopals-solutions/blob/master/aes/block.go
- https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf
- https://crypto.stackexchange.com/questions/1978/how-big-an-rsa-key-is-considered-secure-today
- https://crypto.stackexchange.com/questions/2402/how-to-solve-mixcolumns
- [creel](https://www.youtube.com/watch?v=dRYHSf5A4lw)
- https://www.samiam.org/galois.html

## (C) Shift rows

> The ShiftRows step operates on the rows of the state; it cyclically shifts the bytes in each row by a certain offset. For AES, the first row is left unchanged. Each byte of the second row is shifted one to the left. Similarly, the third and fourth rows are shifted by offsets of two and three respectively. [..] The importance of this step is to avoid the columns being encrypted independently, in which case AES would degenerate into four independent block ciphers.  
[Wikipedia: Advanced Encryption Standard: The ShiftRows step](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard#The_ShiftRows_step)

A state like this:
```asciiart
 | s0  s4  s8  s12 |
 | s1  s5  s9  s13 |
 | s2  s6  s10 s14 |
 | s3  s7  s11 s15 |
```
turns into:
```asciiart
 | s0  s4  s8  s12 | (no change)
 | s5  s9  s13  s1 | ←
 | s10 s14  s2  s6 | ←←
 | s15  s3  s7 s11 | ←←←
```
It can be visualised like this, where the arrows are elements that "wrapped around" from the other side:
```asciiart
 | o o o o |
 | o o o ← |
 | o o ← ← |
 | o ← ← ← |
```

Programmatically, it's usually done as naïvely as this:
```javascript
function shiftrows(s) {
  r = new Uint8Array(16);
  [r[0], r[4], r[ 8], r[12]] = [s[ 0], s[ 4], s[ 8], s[12]];
  [r[1], r[5], r[ 9], r[13]] = [s[ 5], s[ 9], s[13], s[ 1]];
  [r[2], r[6], r[10], r[14]] = [s[10], s[14], s[ 2], s[ 6]];
  [r[3], r[7], r[11], r[15]] = [s[15], s[ 3], s[ 7], s[11]];
  return r;
}
```
(As a copy. To do it inplace change all the `r` on the left side to `s` and the `s` on the right side to `r`, and remove the `return r` line.)

Or avoid the extra array allocation by using temp variables and writing to the same array.

Or copy the initial array and only change the cells that need to be changed (1→5, 5→9, 9→13, 13→1, 2→10, 6→14, 10→2, 14→6, 3→15, 7→3, 11→7, 15→11):
```javascript
function shiftrows(s) {
  c = s.slice();
  [s[ 1], s[5], s[ 9], s[13]] = [c[ 5], c[ 9], c[13], c[ 1]];
  [s[ 2], s[6], s[10], s[14]] = [c[10], c[14], c[ 2], c[ 6]];
  [s[ 3], s[7], s[11], s[15]] = [c[15], c[ 3], c[ 7], c[11]];
}
```

For the inverse, you don't just reverse the indices between the two arrays, because the indices will now be different. Since the real indices are still, same as ever, starting from the first element being at index 0 without regard to what it was at the start. So we're starting again from a "clean slate":
```asciiart
 | s0  s4  s8  s12 |
 | s1  s5  s9  s13 |
 | s2  s6  s10 s14 |
 | s3  s7  s11 s15 |
```
and shift in the opposite direction:
```asciiart
 | s0  s4  s8  s12 | (no change)
 | s13  s1  s5  s9 | →
 | s10 s14  s2  s6 | →→
 | s15  s3  s7 s11 | →→→
```

A single function that can do both (pass -1 as multiplier for the inverse case):
```javascript
function shiftrows(s, multiplier=1) {
  c = s.slice();
  for (let i = 1; i < 4; i++) {
    shift = i * multiplier;
    for (let j = 0; j < 4; j++) {
      let index = (i+j*4 + shift*4) % 16;
      index = index<0 ? index+16 : index;  // normalise for negative shift
      s[i+j*4] = c[index];
    }
  }
}
```
I saw an algorithm like this on [Moserware's `ShiftRows.cs`](https://github.com/moserware/AES-Illustrated/blob/master/Layers/ShiftRows.cs), except he was doing it in a much more object-oriented way.

## (D) Mix columns
This is the most complicated step, and its purpose is diffusion (rounds being able to affect rounds that follow), that's why we don't do it in the last round.

> In the MixColumns step, the four bytes of each column of the state are combined using an invertible linear transformation.  
[Wikipedia: Advanced Encryption Standard: The MixColumns step](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard#The_MixColumns_step)

[Wikipedia: Rijndael MixColumns](https://en.wikipedia.org/wiki/Rijndael_MixColumns)

> The MixColumns() transformation operates on the State column-by-column, treating each column as a four-term polynomial as described in Sec. 4.3. The columns are considered as polynomials over GF(2²) and multiplied modulo x⁴+1 with a fixed polynomial a(x), given by  
a(x) = {03}x³+{01}x²+{01}x+{02}.

this can be written as multiplying each column by the matrix:

```asciiart
 02 03 01 01
 01 02 03 01
 01 01 02 03
 03 01 01 02
```

https://www.utc.fr/~wschon/sr06/txPHP/aes/MixColumn/MixColumn.php

cf sam trenholme
https://www.samiam.org/mix-column.html
https://www.samiam.org/key-schedule.html
https://www.samiam.org/galois.html why is multiplication so complex? is it just polynomial multiplication translated to efficient bitwise operations? (or even bitslicing?)

https://crypto.stackexchange.com/questions/2402/how-to-solve-mixcolumns

```javascript
const invMixColumns = s => {
  for (let i=0; i<4; i++){
    const a = s.slice(i*4, i*4+4);
    s[i*4+0] = gmul(a[0], 14) ^ gmul(a[1], 11) ^ gmul(a[2], 13) ^ gmul(a[3], 9);
    s[i*4+1] = gmul(a[0], 9)  ^ gmul(a[1], 14) ^ gmul(a[2], 11) ^ gmul(a[3], 13);
    s[i*4+2] = gmul(a[0], 13) ^ gmul(a[1], 9)  ^ gmul(a[2], 14) ^ gmul(a[3], 11);
    s[i*4+3] = gmul(a[0], 11) ^ gmul(a[1], 13) ^ gmul(a[2], 9)  ^ gmul(a[3], 14);
  }
};
```

## ------ In other news ------

## Krita `documentinfo.xml` creation date
Since February, I use a [launch script](https://github.com/plu5/dotfiles/blob/main/pm/scripts/pkrita) for Krita which lets you launch into the most recent file you were working on, or create a new file and launch into it. It creates the new file by duplicating a template document. I recently started to use the timelapse recorder feature and discovered that all of my timelapse captures are saved into the same folder: 20140603180818.

Krita files (`.kra`) are archives. One file the archive contains is `documentinfo.xml`, whose `creation-date` field holds the offending date:
```xml
<date>2026-04-28T05:16:59</date>
<creation-date>2014-06-03T18:08:18</creation-date>
```
Since I copy the same file for each document, `creation-date` is always that date in 2014.

Fortunately, simply deleting this file from the template we make copies of resolves the problem; Krita just creates a new `documentinfo.xml` if it's not there when we save, thus each duplicate that we make will get its own creation date when it's first saved.

This was on the 27th.

## Krita timelapsemod
Also on the 27th: Since I had captures from several different files in the same folder, I needed a way to separate them. I wrote a script to that end, [timelapsemod](https://github.com/plu5/dotfiles/blob/main/pm/scripts/timelapsemod). It has a somewhat similar interface to onedriveverrer from [devlog 11](/devlog/11). You can list, move, delete, and renumber captures in the KritaRecorder folder (expanded from `~/KritaRecorder` by default).

For example, my command to sort out my situation was `timelapsemod mv 20140603180818 swat --range 0 4987 --renumber` (move captures 0 to 4987 in 20140603180818 to a new folder swat, and renumber the remaining captures so they start from 0000000).

If you don't give a range it would move the entire folder (useful if you need to rename it). `rm` works the same way (you can delete a range or an entire folder. and there is also confirmation to avoid having accidentally done rm instead of mv for example, which can be skipped with `--noconfirm`).

Additionally, by default when moving files it will verify that no file is in the destination path to avoid overwriting files unwantedly on Linux (on Windows moving files will by default not overwrite without needing any extra verification on the programmer's part. but I added a verification anyway for the sake of Linux.) There is the argument `--force` to force moving files regardless of if there exists something in the destination path, which will overwrite anything existing on both Linux and Windows (by using `os.replace` instead of `os.rename`).

It's a standalone script that doesn't need Krita (relies by default on `~/KritaRecorder` being present, but you can specify any folder with argument `--path`).

## emacs-doentry updates
On the 05-05 I added some things I wanted to add to [doentry-mode](https://github.com/plu5/emacs-doentry) for a while.

doentry files are like XML mixed with markdown (in one particular field).

(1) heading navigation

Wanted to be able to navigate between headings with C-c C-n and C-c C-p like you can in org-mode and markdown-mode.

I saw markdown-mode uses `outline-next-visible-heading` and `outline-previous-visible-heading` with functions to wrap them.

My first approach was just to set `outline-regexp` and bind these two outline functions.

It is maybe overkill for our purposes, but I yoinked this regexp from markdown-mode:
```elisp
;; taken from markdown-mode `markdown-regex-header'
(defconst doentry-mode-header-regexp
  "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]+#+\\)?\\)$"
  "Regexp identifying Markdown headings.
Group 1 matches the text of a setext heading.
Group 2 matches the underline of a level-1 setext heading.
Group 3 matches the underline of a level-2 setext heading.
Group 4 matches the opening hash marks of an atx heading and whitespace.
Group 5 matches the text, without surrounding whitespace, of an atx heading.
Group 6 matches the closing whitespace and hash marks of an atx heading.")
```

In define-derived-mode:
```elisp
  (setq-local outline-regexp doentry-mode-header-regexp)
```
In `doentry-mode-map`:
```elisp
   (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
   (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
```

This works, but if we're on the last heading and do C-c C-n, it goes all the way to the end of the buffer. I wanted to have "stops" at the start/end of the markdown text, before going all the way to the end, and likewise with C-c C-p and the start. I already have this functionality in `doentry-mode-beginning-of-buffer` and `doentry-mode-end-of-buffer` so I just wrote wrapper functions to use them if we are before the first heading or after the last heading.
```elisp
(defun doentry-mode-before-first-heading-p ()
  (not
   (save-excursion
     (beginning-of-line)
     (re-search-backward doentry-mode-header-regexp nil t))))

(defun doentry-mode-after-last-heading-p ()
  (not
   (save-excursion
     (end-of-line)
     (re-search-forward doentry-mode-header-regexp nil t))))

(defun doentry-mode-next-heading (arg)
  (interactive "p")
  (if (doentry-mode-after-last-heading-p)
      (doentry-mode-end-of-buffer)
    (outline-next-visible-heading arg)))

(defun doentry-mode-previous-heading (arg)
  (interactive "p")
  (if (doentry-mode-before-first-heading-p)
      (doentry-mode-beginning-of-buffer)
    (outline-previous-visible-heading arg)))
```
Note that I'm doing `beginning-of-line` or `end-of-line` first before the regexp search, as otherwise depending where the point is on the line we would get different results.

Bindings in `doentry-mode-map`:
```elisp
    (define-key map (kbd "C-c C-n") #'doentry-mode-next-heading)
    (define-key map (kbd "C-c C-p") #'doentry-mode-previous-heading)
```

I had the thought also to just use this regexp search without `save-excursion` instead of the outline functions, since it moves the point already, why do the search twice? But we would have to handle arg ourselves (go forwards/backwards n headings) and there may be other things outline handles for us, so I just left it because it's easier overall.

(2) imenu

markdown-mode does this by doing setq-local imenu-create-index-function in the define-derived-mode body. It even has two different functions based on what the user configured `markdown-nested-imenu-heading-index` (it's a boolean, t by default, i.e. it creates a nested imenu by default).

I didn't want nesting and didn't need something complicated, so instead of a function we can just set [`imenu-generic-expression`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Imenu.html):
```elisp
  (setq-local imenu-generic-expression
              `((nil ,doentry-mode-header-regexp 0)))
```
But I noticed that if the point is before any heading, imenu-list marks me as being on the second heading. So I decided to add an entry for the beginning of the entry text:
```elisp
(defcustom doentry-mode-beg-entry-regexp
  "<key>\\(Entry Text\\)</key>[\n\t ]*<string>"
  "Regexp to find the beginning of the Entry Text element
(element that contains the markdown contents of the entry).
Used for navigation and imenu."
  :type 'regexp
  :group 'doentry-mode)
```
```elisp
  (setq-local imenu-generic-expression
              `((nil ,doentry-mode-beg-entry-regexp 1)
                (nil ,doentry-mode-header-regexp 0)))
```
It still has the problem that if we're before the beginning of the entry text, it will show us as being on the 2nd entry in imenu, which is the first heading. This is not as critical of a problem to me, so I left it.

{% include note.html content='
> [!NOTE]
> Bug: It\'s problematic to use match group 0 there because the header regexp we\'re using can match several lines (in the case of "setext headings" i.e. headings with an underline composed of `-` or `=` characters) which will result in a broken imenu index. Since there are different match groups for the text of atx headings and settext headings, to fix this we either have to change the regexp or switch to defining `imenu-create-index-function` instead of `imenu-generic-expression`.
' %}

(3) generalised C-c C-c

doentry-mode has C-c C-c binding for adding things to the entry, where you can give a numerical argument to change the behaviour (for example, add a heading beforehand as well, add a separator). I myself can't decide what these should be and change my mind, so they should be configurable.
```elisp
(defcustom doentry-mode-entry-time-string
  "[%H:%M] "
  "Format of timestamp inserted on `doentry-mode-add-to-log'.
Set this to an empty string to disable inserting timestamps."
  :type 'string
  :group 'doentry-mode)

(defcustom doentry-mode-add16
  "## "
  "Inserted before the entry on C-u C-u `doentry-mode-add-to-log'."
  :type 'string
  :group 'doentry-mode)

(defcustom doentry-mode-add64
  "-----

## "
  "Inserted before the entry on C-u C-u C-u `doentry-mode-add-to-log'."
  :type 'string
  :group 'doentry-mode)
```
The function itself:
```elisp
(defun doentry-mode-add-to-log (arg)
  "Add a new timestamped entry to the log.
with C-u does not insert timestamp.
with C-u C-u also inserts a `doentry-mode-add16' before the entry.
with C-u C-u C-u also inserts a `doentry-mode-add64' before the entry.
Doesn't insert anything if entry xml tag not found."
  (interactive "P")
  (when (doentry-mode-end-of-entry-string)
    (beginning-of-line)
    (open-line 2)
    (when (= 16 (prefix-numeric-value arg))
      (insert doentry-mode-add16)
      (newline))
    (when (= 64 (prefix-numeric-value arg))
      (insert doentry-mode-add64)
      (newline))
    (unless (= 4 (prefix-numeric-value arg))
      (insert (format-time-string doentry-mode-entry-time-string)))))
```

Now that I'm thinking about it, this function should probably be called add to entry not add to log, this may be confusing. I really don't think I have any users, so I could just stealth-change it. But the docstring is "Add a new timestamped entry to the log." and if we change it to entry, it would be what, "Add a new timestamped entry to the entry"? That's weird. Change the first "entry" to a synonym, like "addition"? "update"? [I went with update](https://github.com/plu5/emacs-doentry/commit/dcfc285b39c17c63491485632191d66cf54fcad7) even though I don't like it that much.

## Custom org-meta-return
05-06. It's also a emacs-doentry change. I have been using org-meta-return by just suppressing the warning (it doesn't like running in other modes):
```elisp
(defun doentry-mode-meta-return ()
  (interactive)
  (let ((warning-suppress-types '((org-element))))
    (org-meta-return)))
```
But I think there are issues with this, not sure what exactly, but I noticed after some time [delete-selection-mode stops working](https://stackoverflow.com/questions/14954490/emacs-delete-selection-mode-disables-itself) for example. It seems to also scan the contents of the buffer and get upset if it finds something it wasn't expecting; I sometimes get org parser error, and:

> rx--translate-bounded-repetition: rx ‘**’ range error

Since we don't use the same headings as org-mode anyway, it would make sense to use a different function.

What it needs to do:
1. If pressed while the point is in an item of a list, create a new line with a new list item with the same bullet and indentation, containing whatever was after the point in the line (i.e. if the point was in the middle of a line then the text after the point should be in the new item)
2. If pressed while the point is in a heading, same behaviour as 1 except with the heading level markers instead of a bullet (and no indentation)

List bullets: `-`, `+`, `*` (followed by a space)

Headings: any number of `#` (followed by a space)

I forgot to mention also the case of a numbered list. In that case we need to also increment the "bullet".

Let's start with a function that can tell us if we're in an item of a list:
```elisp
(defun doentry-mode-in-list-item-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[\t ]*[-\\+\\*] ")))
```
`+` and `*` need to be escaped in regexp.

We also need the bullet, so we could rewrite this function to give us the bullet if we're on a list item, or nil if we're not. `looking-at` is like `looking-at-p` except it also sets the match data. So we could use `looking-at` and change our regexp to put the bullet in its own match group:
```elisp
(defun doentry-mode-list-item-bullet-or-nil ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[\t ]*\\([-\\+\\*]\\) ")
      (match-string-no-properties 1))))
```
We get for example `"-"` when it's a list item that begins with `-`. But... we also need the indentation actually, if we want to create a list item with the same indentation. I think org-mode calculates what the indentation needs to be itself and "normalises" it, but I don't want to do that, I'd rather just go with whatever indentation the user is using (it's a lot simpler than having to calculate the structure of the list, and also as a user I find it less annoying/invasive anyway).
```elisp
(defun doentry-mode-list-item-prefix-or-nil ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[\t ]*[-\\+\\*] ")
      (match-string-no-properties 0))))
```

Which gives us the first version of our simple alternative to org-meta-return:
```elisp
(defun doentry-mode-meta-return ()
  (interactive)
  (let ((item-prefix (doentry-mode-list-item-prefix-or-nil)))
    (when item-prefix
      (newline)
      (insert item-prefix))))
```

It already completely achieves objective 1, we don't need to do anything special to get the text after the point to be in the new list item, newline already does that.

One thing that's different is if there was whitespace before the point. If we had:
```text
- hello world
```
and put the point on the w and pressed meta return:
```text
- hello 
- world
```
with the whitespace remaining at the end of the first list item (after hello). In org-mode, this whitespace gets removed. We could add this little loop before the newline:
```elisp
      (while (eq (char-before) ?\s)
        (delete-char -1))
```

Now onto objective 2. It's pretty much the same, except we don't want to allow indentation before the "bullet". We could just change the regexp to add an or case: `"^[\t ]*[-\\+\\*] \\|^[#]+ "`.

The only thing left is to handle numbered lists. Another or case to our regexp: `"^[\t ]*[-\\+\\*] \\|^[#]+ \\|^[\t ]*[0-9]+\\. "`

In `doentry-mode-meta-return` we could naïvely handle the incrementation like this:
```elisp
      (let ((split-prefix (split-string item-prefix "\\.")))
        (when (string-match-p "^[0-9]+$" (car split-prefix))
          (setf (car split-prefix)
                (number-to-string (1+ (string-to-number (car split-prefix)))))
          (setq item-prefix (mapconcat 'identity split-prefix "."))))
```
The use of `setf` to set a list item inplace I got from [Dan 2014](https://emacs.stackexchange.com/a/3398). `mapconcat` as the inverse of `split-string` I got from [N.N. 2012](https://stackoverflow.com/questions/12999530/is-there-a-function-that-joins-a-list-of-strings-into-a-delimited-string#comment17637027_12999828). In `mapconcat` we dont need to escape the `.` because there it's not a regexp.

If we add an item in the middle of the list all the subsequent items have to be renumbered. This is a bit of a pain. And I think numbered lists in markdown can be very flexible and you can have empty lines between them, at least in Github-flavoured markdown.

Naïve implementation that only supports numbered lists where each item is only 1 line:
```elisp
(defun doentry-mode-renumber-list-after-point (offset)
  (save-excursion
    (forward-line 1)
    (while (looking-at "^[\t ]*\\([0-9]+\\)\\. ")
      (let* ((n (string-to-number (match-string 1)))
             (replacement (number-to-string (+ n offset))))
        (replace-match replacement t t nil 1))
      (forward-line 1))))
```
and call `(doentry-mode-renumber-list-after-point 1)` in `doentry-mode-meta-return` if it's a numbered list.

The whole thing:
```elisp
(defun doentry-mode-list-item-prefix-or-nil ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[\t ]*[-\\+\\*] \\|^[#]+ \\|^[\t ]*[0-9]+\\. ")
      (match-string-no-properties 0))))

(defun doentry-mode-renumber-list-after-point (offset)
  (save-excursion
    (forward-line 1)
    (while (looking-at "^[\t ]*\\([0-9]+\\)\\. ")
      (let* ((n (string-to-number (match-string 1)))
             (replacement (number-to-string (+ n offset))))
        (replace-match replacement t t nil 1))
      (forward-line 1))))

(defun doentry-mode-meta-return ()
  (interactive)
  (let ((item-prefix (doentry-mode-list-item-prefix-or-nil)))
    (when item-prefix
      (let ((split-prefix (split-string item-prefix "\\.")))
        ;; numbered list
        (when (string-match-p "^[0-9]+$" (car split-prefix))
          (setf (car split-prefix)
                (number-to-string (1+ (string-to-number (car split-prefix)))))
          (setq item-prefix (mapconcat 'identity split-prefix "."))
          (doentry-mode-renumber-list-after-point 1)))
      (while (eq (char-before) ?\s)
        (delete-char -1))
      (newline)
      (insert item-prefix))))
```

I'm going to add docstrings and be clear about the limited support for numbered lists.

[Commit 4588971](https://github.com/plu5/emacs-doentry/commit/45889718cf13dd7db93559d8e25b7144e5dfb981)

Next it would be nice to implement something like `org-metaright` and `org-metaleft` (promote or demote items or headings), but I'll leave this to another day (or never).

And by the way something else `org-meta-return` does (and also the aforementioned promote and demote functions) is something with tables. I never use tables so I don't know. But ideally doentry-mode should be capable of that too if we are really to replicate the functionality of `org-meta-return`.

I'll add a roadmap section to the readme with these things.

[Moserware]: https://www.moserware.com/2009/09/stick-figure-guide-to-advanced.html

{% include fin.html %}
