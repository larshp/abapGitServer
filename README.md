[![abaplint](https://abaplint.org/badges/larshp/abapGitServer)](https://abaplint.org/project/larshp/abapGitServer)

# abapGitServer
Git server implemented in ABAP

## Install

via [abapGit](http://www.abapgit.org)

## Setup

by activating the ServiceName ZABAPGITSERVER in transaction SICF

## Start

to use it via the web frontend by running transaction ZABAPGITSERVER (or right click on the SICF service ZABAPGITSERVER and choose Test Service)

### News
2018-02-25: Support for tags added, use conversion program ZAGS_MIGRATION_03 to convert old repositories

2018-01-21: Field ADLER32 added in database table ZAGS_OBJECTS, use conversion program ZAGS_MIGRATION_02 to convert old repositories

2018-01-13: Make sure you have the latest development version of abapGit installed

2017-03-12: Key field REPO added in database table ZAGS_OBJECTS, use conversion program ZAGS_MIGRATION_01 to convert old repositories

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

### Use with abapGit
abapGit is your git client and the abapGitServer our server. We first have to setup the [ssl connection](https://docs.abapgit.org/guide-ssl-setup.html) with the root certificate of the sap system the abapGitServer is running on.
After that the abapGitServer can be used like every other git server.

### External libraries

Library   | Version | License
:------------ | :------------ | :------------
[spinkit](https://github.com/tobiasahlin/SpinKit) | 1.2.5 | MIT
[react](https://github.com/facebook/react) | 15.2.1 | Facebook BSD + Patents
[babel-standalone](https://github.com/babel/babel-standalone) | 6.23.1 | MIT
[babel-polyfill](https://github.com/babel/babel/tree/master/packages/babel-polyfill) | 6.23.0 | MIT
[react-router](https://github.com/ReactTraining/react-router) | 2.5.2 | MIT
[history](https://github.com/ReactTraining/history) | 2.1.2 | MIT
[prism](https://github.com/PrismJS/prism) | 1.6.0 | MIT
[octicons](https://github.com/primer/octicons/) | 3.5.0 | MIT
[jsdiff](https://github.com/kpdecker/jsdiff) | 2.2.3 | BSD-3-Clause
[jquery](https://github.com/jquery/jquery) | 2.2.3 | MIT
[diff2html](https://github.com/rtfpessoa/diff2html) | 2.3.0 | MIT

### Screenshots
[See wiki](https://github.com/larshp/abapGitServer/wiki/Screenshots)
