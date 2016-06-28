[![Build Status](https://travis-ci.org/larshp/abapGitServer.svg?branch=master)](https://travis-ci.org/larshp/abapGitServer)
[![devDependency Status](https://david-dm.org/larshp/abapGitServer/dev-status.svg)](https://david-dm.org/larshp/abapGitServer#info=devDependencies)

# Work in progress

## abapGitServer
Git server implemented in ABAP

Install via abapGit

Requirements:
- https://github.com/larshp/ABAP-Swagger
- https://github.com/larshp/abapGit

Scope:
- Web interface: browse/create repository
- No tags, no submodules, no blame
- A lot of code from abapGit can be reused
- Currently 7.50 is required, backport is planned
- Only tested with abapGit as client, support for normal git clients is planned

Use cases:
- Automatic backup of objects
- Increased visibility over changes in system
