[![Build Status](https://travis-ci.org/larshp/abapGitServer.svg?branch=master)](https://travis-ci.org/larshp/abapGitServer)
[![devDependency Status](https://david-dm.org/larshp/abapGitServer/dev-status.svg)](https://david-dm.org/larshp/abapGitServer?type=dev)
[![abaplint](http://abaplint.org/badges/larshp/abapGitServer)](http://abaplint.org/project/larshp/abapGitServer)

# abapGitServer
Git server implemented in ABAP

Install via [abapGit](http://www.abapgit.org)

Start web frontend via SICF service ZGIT

### Requirements
- https://github.com/larshp/ABAP-Swagger
- https://github.com/larshp/abapGit

### Version requirement
see https://github.com/larshp/ABAP-Swagger

### Scope
- Web interface: browse/create repository
- No tags, no submodules, no blame
- Primarily tested with abapGit as client

### Use cases
- Automatic backup of objects
- Increased visibility over changes in system

Works with 'git pull' from command line, so all objects can be exported to a different git server if needed, [guide](https://help.github.com/articles/importing-a-git-repository-using-the-command-line/)
