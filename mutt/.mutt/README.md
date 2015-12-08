Mutt
====

Requirements:

- msmtp
- offlineimap
- goobook?
- sqlite
- mutt-patched (Debian)
- notmuch

Account file
------------

```
# vim: filetype=muttrc

set imap_user = 'username'
set imap_pass = 'password'
```

Store secret in keyring:

```
secret-tool store --label=email account <name>
```
When prompted, enter account secret password.
