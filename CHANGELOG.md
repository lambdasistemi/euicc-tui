# Changelog

## Unreleased

## [0.1.1.0](https://github.com/lambdasistemi/euicc-tui/releases/tag/v0.1.1.0) (2026-09-26)

### Features

* decode lpac output and classify reader failures ([fb200f0](https://github.com/lambdasistemi/euicc-tui/commit/fb200f0834abb6e49f8ae71f07ad2e6c9ad2bd03))
* parse activation codes and keep the matching ID secret ([51b32a5](https://github.com/lambdasistemi/euicc-tui/commit/51b32a53f18fc9e1c8bddc8a1fdc09445d47b4f1))
* define the lpac command set with the PC/SC backend forced ([7ec99f1](https://github.com/lambdasistemi/euicc-tui/commit/7ec99f175be0482ca2c073da2b72bb755b7c08b4))
* run jobs through an lpac runner and reload the card ([28f43c8](https://github.com/lambdasistemi/euicc-tui/commit/28f43c8b590364b8eec570ed16a73b1a44c6fa1c))
* model the UI as a pure state machine ([20bfb51](https://github.com/lambdasistemi/euicc-tui/commit/20bfb512b13df8b4f44be4b27431c6fea70cce69))
* brick terminal front end ([40e4723](https://github.com/lambdasistemi/euicc-tui/commit/40e4723faee2a57d41feb91836a1d573bf6414f1))
* parse the confirmation-code flag of activation codes ([c5acf21](https://github.com/lambdasistemi/euicc-tui/commit/c5acf2133501911c0021d594f41bc790c8f2f812))
* nickname and confirmation-code lpac commands ([c34f71c](https://github.com/lambdasistemi/euicc-tui/commit/c34f71cffcf4d096b1dd122ccb892233149a370e))
* decode activation codes from QR images ([f05f3ed](https://github.com/lambdasistemi/euicc-tui/commit/f05f3ed975d1949c8b4ecc31faee6a1996bedbd1))
* guided install opens and closes ([8fe911b](https://github.com/lambdasistemi/euicc-tui/commit/8fe911b74c5b1c338f6e6e2ba942bc6b1fbaab7e))
* read the purchase QR in the guided install ([1ef8784](https://github.com/lambdasistemi/euicc-tui/commit/1ef878427bf82abb0a2fb628b3d3413260e4e5f1))
* guided install walks download to closing instruction ([c113275](https://github.com/lambdasistemi/euicc-tui/commit/c1132754650a2c8181aa408d9923d81f2e8a0689))
* nickname editing and the safety property over the new jobs ([5a59460](https://github.com/lambdasistemi/euicc-tui/commit/5a5946048c12b265a3f68ad4a44220b4407b5ac7))
* a directory-listing job for the QR picker ([88de211](https://github.com/lambdasistemi/euicc-tui/commit/88de21127ee63cb9840af71e95d14aff54a9da7d))
* pick the QR image with a cursor, not a typed path ([473dac7](https://github.com/lambdasistemi/euicc-tui/commit/473dac78181d1abe1e70c6f72a229029b94393a2))
* guided install ends on the profile list ([5a4a623](https://github.com/lambdasistemi/euicc-tui/commit/5a4a62365d9b98bea9e3cf13dd9ce3ee2727c908))
* guarded profile delete ([3536942](https://github.com/lambdasistemi/euicc-tui/commit/3536942544c4eea48aae6624eaa963c56d00321e))
* redesigned screens, help on ?, striped tables ([c208571](https://github.com/lambdasistemi/euicc-tui/commit/c208571b8332a9984e934f250e9adeaef0ba2097))
* mouse support ([f8e9701](https://github.com/lambdasistemi/euicc-tui/commit/f8e9701af12c23b699be1a421def6ac7539e9a76))
* clickable key hints and form fields ([bc5c8fc](https://github.com/lambdasistemi/euicc-tui/commit/bc5c8fc4a6bf2c50705aad3596df4c35b01dfb02))
* light and dark themes, following the terminal ([28ef1ad](https://github.com/lambdasistemi/euicc-tui/commit/28ef1ad4e886b7b609d4b3525de2c97d7aeaae4d))
* light palette on the terminal's background, live theme switch ([64786ae](https://github.com/lambdasistemi/euicc-tui/commit/64786aef1b98d0738af0a7f00f9e7232b888aa12))
* QR picker lists the newest first ([1423dd1](https://github.com/lambdasistemi/euicc-tui/commit/1423dd13311899b1d1349bce7088d0cadf5d1290))
* Linux release packages (.deb, AppImage) (#7) ([bf7ea54](https://github.com/lambdasistemi/euicc-tui/commit/bf7ea54730d4190e805c95b604ce10812b6fe30d))

### Bug Fixes

* show the QR field in the guided install ([f276d86](https://github.com/lambdasistemi/euicc-tui/commit/f276d8687bbf4bb3c966f08ac47080c86b8bd6b1))
* light table rows, white and pale yellow ([3053e8d](https://github.com/lambdasistemi/euicc-tui/commit/3053e8d72ef4238068c7c2bed312a432ef2cb141))
* no color for the enabled row; the dot marks it ([c084d31](https://github.com/lambdasistemi/euicc-tui/commit/c084d317b4ecfb4ed5ea97de3d4056c704fdbb03))
* dialogs via brick's Dialog, plain key names, ? always hinted ([b74d465](https://github.com/lambdasistemi/euicc-tui/commit/b74d46552ea97e18bcc72f7af33eae1c7bfe754c))
* draw clickable key hints as buttons ([69fcdc9](https://github.com/lambdasistemi/euicc-tui/commit/69fcdc977ab27024c029a4bcbc4e2e079685b2d9))
* light selection for rows and the active tab ([a2a8d3d](https://github.com/lambdasistemi/euicc-tui/commit/a2a8d3dc4d18ed11a0ec618adffed96ecae3dc5e))
* QR picker lists only folders and images, and goes up from the start ([fa2b28c](https://github.com/lambdasistemi/euicc-tui/commit/fa2b28c63b7e48af8a6147416fbbbdbd1a8a262a))

