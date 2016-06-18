[![Build Status](https://travis-ci.org/larshp/abapGitServer.svg?branch=master)](https://travis-ci.org/larshp/abapGitServer)

# not working yet

## abapGitServer
Git server implemented in ABAP

Scope:
- Limited to work with abapGit, later it can be extended to work with "real" git clients
- Web interface: browse/create repository
- No tags, no submodules, no blame
- A lot of code from abapGit can be reused, but I'd like to keep the codebase/projects separated
- Install via abapGit
- As SICF service

Currently 7.50 is required, backport is planned. 

abapGit must be installed as ZABAPGIT report for abapGitServer to work

Only tested with abapGit as client, support for normal git clients is planned
