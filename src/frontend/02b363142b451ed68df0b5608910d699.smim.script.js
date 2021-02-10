const base = '/sap/zabapgitserver';
const Link = ReactRouter.Link;

class Octicons {
  static branch() {
    return (<span className="octicon octicon-git-branch"></span>);
  }
            
  static repo() {
    return (<span className="octicon octicon-repo"></span>);
  }    

  static file() {
    return (<span className="octicon octicon-file-code"></span>);
  }       

  static directory() {
    return (<span className="octicon octicon-file-directory"></span>);
  } 
            
  static commit() {
    return (<span className="octicon octicon-git-commit"></span>);
  }   
            
  static plus() {
    return (<span className="octicon octicon-plus"></span>);
  }    
            
  static pencil() {
    return (<span className="octicon octicon-pencil"></span>);
  }   

  static clippy() {
    return (<span className="octicon octicon-clippy"></span>);
  }            
}

function cloneURL(repo) {
    return window.location.origin + base + "/git/" + repo + ".git";
}

class Time {
  static parse(time) {
    return new Date(parseInt(time) * 1000);
  }
  
  static ago(date) {
    var seconds = Math.floor((new Date() - date) / 1000);

    var interval = Math.floor(seconds / 31536000);

    if (interval > 1) {
      return interval + " years ago";
    }
    interval = Math.floor(seconds / 2592000);
    if (interval > 1) {
      return interval + " months ago";
    }
    interval = Math.floor(seconds / 86400);
    if (interval > 1) {
      return interval + " days ago";
    }
    interval = Math.floor(seconds / 3600);
    if (interval > 1) {
      return interval + " hours ago";
    }
    interval = Math.floor(seconds / 60);
    if (interval > 1) {
      return interval + " minutes ago";
    }
    return Math.floor(seconds) + " seconds ago";
  }
}

function handleError(evt, callback, json) {
  if (evt.target.status === 200) {
    if (json === true) {
      callback(JSON.parse(evt.target.responseText).DATA);
    } else {
      callback(evt.target.responseText);
    }
  } else {
    alert("REST call failed, status: " + evt.target.status);
  }
}

class REST {
  static root = base + "/rest/";

  static listRepositories(callback) {
    this.get("list", callback);
  }

  static createRepository(data, callback) {
    this.post("create", callback, data);
  }
      
  static editRepository(data, callback) {
    this.put("edit", callback, data);
  }      

  static listBranches(repoName, callback) {
    this.get("branches/" + repoName, callback);    
  }
  
  static listFiles(repoName, branch, path, callback) {
    this.get("tree/" + repoName + "/" + branch + path.replace(/#/g, "%23"), callback);
  }

  static listCommits(repoName, branch, callback) {
    this.get("commits/" + repoName + "/" + branch, callback);
  }

  static readBlob(repoName, branch, filename, callback) {
    const url = "blob/" + repoName + "/" + branch + "/" + filename.replace(/#/g, "%23");
    this.get(url, callback, false);
  }
   
  static readHistory(repoName, branch, filename, callback) {
    const url = "history/" + repoName + "/" + branch + "/" + filename.replace(/#/g, "%23");
    this.get(url, callback, true);
  }
   
  static readBlobSHA1(repoName, sha1, callback) {
    const url = "blob/" + repoName + "/" + sha1;
    this.get(url, callback, false);
  }      

  static readCommit(repoName, sha1, callback) {
    const url = "commit/" + repoName + "/" + sha1;
    this.get(url, callback);
  }

  static get(folder, callback, json = true) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback, json); });
    oReq.open("GET", this.root + folder);
    oReq.send();
  }

  static post(folder, callback, data) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback, false); });
    oReq.open("POST", this.root + folder);
    oReq.send(JSON.stringify(data));
  }
      
  static put(folder, callback, data) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback, false); });
    oReq.open("PUT", this.root + folder);
    oReq.send(JSON.stringify(data));
  }      
}

class NoMatch extends React.Component {
  render() {
    return (<div><h1>404</h1><h3>Router, no match</h3></div>);
  }
}

class CommitList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], spinner: true };
    REST.listCommits(props.params.repo, 
                     props.params.branch, 
                     this.update.bind(this));
  }

  update(d) {
    this.setState({data: d, spinner: false});
  }    

  commit(e) {
    let ago = Time.ago(Time.parse(e.COMMITTER.TIME));
    return (
      <div>
      <hr />
      <table>
      <tr>
      <td>Description:</td>
      <td><Link to={this.props.params.repo + "/commit/" + e.SHA1}>{e.TEXT}</Link></td>
      </tr>
      <tr>
      <td>Time:</td>
      <td>{ago}</td>
      </tr>
      <tr>
      <td>Name:</td>
      <td><Link to={"/user/" + e.COMMITTER.NAME}>{e.COMMITTER.NAME}</Link></td>
      </tr>
      </table>
      </div>);
  }

  render() {
    let total = this.state.data.length;
    let view = Math.min(total, 1000);
    if (total !== view) {
      this.state.data.splice(view);
    }
      
    return (<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>Commits</h1>
      {total} commits total, {view} commits listed<br /><br />
      {this.state.spinner?<Spinner />:this.state.data.map(this.commit.bind(this))}
      </div>);
  }
}
            
class Spinner extends React.Component {
  render() {
    let rand = Math.floor(Math.random() * (11 - 1)) + 1;

    switch (rand) {
      case 1:
        return (<div className="sk-rotating-plane"></div>);
      case 2: 
        return (<div className="sk-double-bounce">
        <div className="sk-child sk-double-bounce1"></div>
        <div className="sk-child sk-double-bounce2"></div>
        </div>);
      case 3:         
        return (<div className="sk-wave">
        <div className="sk-rect sk-rect1"></div>
        <div className="sk-rect sk-rect2"></div>
        <div className="sk-rect sk-rect3"></div>
        <div className="sk-rect sk-rect4"></div>
        <div className="sk-rect sk-rect5"></div>
        </div>);
      case 4:         
        return (<div className="sk-wandering-cubes">
        <div className="sk-cube sk-cube1"></div>
        <div className="sk-cube sk-cube2"></div>
        </div>);
      case 5:
        return (<div className="sk-spinner sk-spinner-pulse"></div>);
      case 6:
        return (<div className="sk-chasing-dots">
        <div className="sk-child sk-dot1"></div>
        <div className="sk-child sk-dot2"></div>
        </div>);
      case 7:
        return (<div className="sk-three-bounce">
        <div className="sk-child sk-bounce1"></div>
        <div className="sk-child sk-bounce2"></div>
        <div className="sk-child sk-bounce3"></div>
        </div>);
      case 8:
        return (<div className="sk-circle">
        <div className="sk-circle1 sk-child"></div>
        <div className="sk-circle2 sk-child"></div>
        <div className="sk-circle3 sk-child"></div>
        <div className="sk-circle4 sk-child"></div>
        <div className="sk-circle5 sk-child"></div>
        <div className="sk-circle6 sk-child"></div>
        <div className="sk-circle7 sk-child"></div>
        <div className="sk-circle8 sk-child"></div>
        <div className="sk-circle9 sk-child"></div>
        <div className="sk-circle10 sk-child"></div>
        <div className="sk-circle11 sk-child"></div>
        <div className="sk-circle12 sk-child"></div>
        </div>);
      case 9:
        return (<div className="sk-cube-grid">
        <div className="sk-cube sk-cube1"></div>
        <div className="sk-cube sk-cube2"></div>
        <div className="sk-cube sk-cube3"></div>
        <div className="sk-cube sk-cube4"></div>
        <div className="sk-cube sk-cube5"></div>
        <div className="sk-cube sk-cube6"></div>
        <div className="sk-cube sk-cube7"></div>
        <div className="sk-cube sk-cube8"></div>
        <div className="sk-cube sk-cube9"></div>
        </div>);
      case 10:
        return (<div className="sk-fading-circle">
        <div className="sk-circle1 sk-circle"></div>
        <div className="sk-circle2 sk-circle"></div>
        <div className="sk-circle3 sk-circle"></div>
        <div className="sk-circle4 sk-circle"></div>
        <div className="sk-circle5 sk-circle"></div>
        <div className="sk-circle6 sk-circle"></div>
        <div className="sk-circle7 sk-circle"></div>
        <div className="sk-circle8 sk-circle"></div>
        <div className="sk-circle9 sk-circle"></div>
        <div className="sk-circle10 sk-circle"></div>
        <div className="sk-circle11 sk-circle"></div>
        <div className="sk-circle12 sk-circle"></div>
        </div>);
      default:
        return (<div className="sk-folding-cube">
        <div className="sk-cube1 sk-cube"></div>
        <div className="sk-cube2 sk-cube"></div>
        <div className="sk-cube4 sk-cube"></div>
        <div className="sk-cube3 sk-cube"></div>
        </div>);
    }
  }
}  
           
class Edit extends React.Component {
  constructor(props) {
    super(props);
    this.state = {description: "", spinner: true };

    REST.listRepositories(this.update.bind(this));      
  }

  update(d) {
    for (let repo of d) {
      if (repo.REPO.NAME === this.props.params.repo) {
        this.setState({description: repo.REPO.DESCRIPTION, spinner: false});        
      }
    }
  }        

  callback(d) {
    this.setState({done: true});
  }
    
  click(e) {
    REST.editRepository(
      {name: this.props.params.repo, description: this.state.description}, 
      this.callback.bind(this));
    this.setState({running: true});
    e.preventDefault();
  }       
    
  changeDesc(e) {
    this.setState({description: e.target.value, spinner: false});
  }    
    
  edit() {
    return (<div>
      <form>
      <table>
      <tr>
      <td>Description:</td>
      <td>
      <input type="text" value={this.state.description} onChange={this.changeDesc.bind(this)} />     
      </td>
      </tr>
      <tr>
      <td colSpan="2">
      <input type="submit" value="Edit" onClick={this.click.bind(this)}/>
      </td>
      </tr>
      </table>
      </form>
      </div>);
  }    
    
  contents() {
    if (this.state.done) {
      return (<div>done</div>);  
    } else if (this.state.running) {
      return (<div>running</div>);
    } else if (this.state.spinner) {
      return (<Spinner />);              
    } else { 
      return this.edit();
    }        
  }
  
  render() {
    return(<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>Edit</h1>
      {this.contents()}
      </div>);
  }
}             
           
class Create extends React.Component {
  constructor() {
    super();
    this.state = {name: "", description: ""};
  }
    
  callback(d) {
    this.setState({done: true});
  }
    
  click(e) {
    REST.createRepository(
      {name: this.state.name, description: this.state.description}, 
      this.callback.bind(this));
    this.setState({running: true});
    e.preventDefault();
  }           
         
  changeName(e) {
    this.setState({name: e.target.value, description: this.state.description});
  }
    
  changeDesc(e) {
    this.setState({name: this.state.name, description: e.target.value});
  }
  
  contents() {
    if (this.state.done) {
      return (<div>done</div>);  
    } else if (this.state.running) {
      return (<div>running</div>);
    } else {    
      return (<form><table border="1">
      <tr>
      <td>Name: </td>
      <td>
      <input type="text" value={this.state.name} onChange={this.changeName.bind(this)} />
      </td>
      </tr>
      <tr>
      <td>Description:</td> 
      <td>
      <input type="text" value={this.state.description} onChange={this.changeDesc.bind(this)} />
      </td>
      </tr>
      <tr>
      <td colSpan="2">
      <input type="submit" value="Create" onClick={this.click.bind(this)}/>
      </td>
      </tr>
      </table></form>);
    }      
  }
  
  render() {
    return(
      <div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>Create</h1>
      {this.contents()}
      </div>);
  }
}         

// todo, rewrite most of this class with diff2html
// see https://github.com/larshp/abapGitServer/issues/21      
class Diff extends React.Component {
  old;
  new;
  
  constructor(props) {
    super(props);

    this.state = {diff: null};
    if (props.old === "") {
      this.old = "";
    } else {
      this.old = null;
      REST.readBlobSHA1(props.repo,
                        props.old,
                        this.oldd.bind(this));
    }
    if (props.new === "") {
      this.new = "";
    } else {
      this.new = null;
      REST.readBlobSHA1(props.repo,
                        props.new,
                        this.newd.bind(this));     
    }     
  }
  
  runDiff() {
    if (this.old !== null && this.new !== null) {
      let diff = JsDiff.createTwoFilesPatch(
        this.props.path + this.props.filename, 
        this.props.path + this.props.filename, 
        this.old, 
        this.new);
      
// hack to make diff2html show added and deleted marker      
      if (this.props.old === "") {
        diff = 'diff --git foo bar\n' +
          'new file mode 100644\n' + 
          diff;
      } else if (this.props.new === "") {
        diff = 'diff --git foo bar\n' +
          'deleted file mode 100644\n' + 
          diff;        
      }        

      var diff2htmlUi = new Diff2HtmlUI({diff});   
      diff2htmlUi.draw('#line-by-line'+this.props.fileNumber, {
        inputFormat: 'json',
//        showFiles: true,
        matching: 'lines'
      });      

      this.setState({diff});
    }
  }
  
  oldd(d) {
    this.old = d;
    this.runDiff();
  }
  
  newd(d) {
    this.new = d;
    this.runDiff();
  }
    
  render() {  
    return (<div>
      <div id={"line-by-line"+this.props.fileNumber}><Spinner /></div>
      </div>);
  }
}      
      
class Commit extends React.Component {
  i = 0;
            
  constructor(props) {
    super(props);
    this.init(props);    
  }
      
  componentWillReceiveProps(props) {
    this.init(props);
  }      

  init(props) {
    this.state = {data: undefined, spinner: true };
    REST.readCommit(props.params.repo, 
                    props.params.sha1, 
                    this.update.bind(this)); 
  }      
      
  update(d) {
    this.setState({data: d, spinner: false});
  }      
 
  single(e) {
    return (<Diff 
      filename={e.FILENAME} 
      path={e.PATH}
      fileNumber={this.i++} 
      repo={ this.props.params.repo }
      old={ e.OLD_BLOB } 
      new={ e.NEW_BLOB } />);
  }
  
  diff() {
    let total = this.state.data.FILES.length;
    let view = Math.min(total, 50);
    if (total !== view) {
      this.state.data.FILES.splice(view);
    }

    return (<div>{ total } total, { this.state.data.FILES.length } shown<br />{
      this.state.data.FILES.map(this.single.bind(this))}</div>);             
  }
  
  parentLink(parent) {
    return (<Link to={this.props.params.repo + "/commit/" + parent}>{parent}</Link>);
  }            
            
  renderCommit() {
    let data = this.state.data;
    return (
      <div>
      <table>
      <tr>
      <td>Name:</td>
      <td><Link to={"/user/"+data.COMMITTER.NAME}>{data.COMMITTER.NAME}</Link></td>
      </tr>
      <tr>
      <td>Description:</td>
      <td>{data.TEXT}</td>
      </tr>
      <tr>
      <td>Parent:</td>
      <td>
      {this.parentLink(data.PARENT)}
      {data.PARENT2?" + ":""}
      {data.PARENT2?this.parentLink(data.PARENT2):""}
      </td>
      </tr>
      <tr>
      <td>Time:</td>
      <td>{Time.ago(Time.parse(data.COMMITTER.TIME))}</td>
      </tr>
      </table>
      <br />
      {this.diff()}
      </div>);
  }      
      
  render() {
    return(<div>
           <Breadcrumb routes={this.props.routes} params={this.props.params} />
           <h1>Commit {this.props.params.sha1}</h1>
           {this.state.spinner?<Spinner />:this.renderCommit()}             
           </div>);
  }
}            
           
class BranchList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], mergeRequest: {}, spinner: true };
    REST.listBranches(props.params.repo, 
                      this.update.bind(this));
  }
  
  update(d) {
    this.setState({data: d, mergeRequest: {}, spinner: false});
  }
  
  single(e) {
    let cla = "";
    if (e.HEAD === "X") {
      cla = "bold";
    }
    
    return (<tr>
      <td>{Octicons.branch()}</td>
      <td>
      <Link className={cla} to={this.props.params.repo + "/files/" + e.NAME}>
      {e.NAME}
      </Link>
      </td>
      <td>
      {Time.ago(Time.parse(e.TIME))}
      </td>
      </tr>);
  }
  
  list() {
    return (<table>{this.state.data.map(this.single.bind(this))}</table>);
  }
 
  createMergeRequest() {
    
  }

  onSelectSource(event) {
    const state = this.state;
    state.mergeRequest.sourceBranch = event.data;  
    this.setState(state);
  }

  onSelectTarget(event) {
    const state = this.state;
    state.mergeRequest.targetBranch = event.data;
    this.setState(state);
  }

  render() {
    return(<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>Branch list</h1>
      Clone URL: {cloneURL(this.props.params.repo)}<br />
      <br />
      <button onClick={this.createMergeRequest.bind(this)}>Create Merge request</button><br />
      From <BranchSelect branches={this.state.data} onselect={this.onSelectSource}/> To <BranchSelect branches={this.state.data} onselect={this.onSelectTarget}/>
      <br />
      {this.state.spinner?<Spinner />:this.list()}                   
      <br />
      <button onClick={this.listMergeRequests.bind(this)}>List merge requests</button>
      <DiffMergeRequest mergeRequest={this.state.mergeRequest}/>
      </div>);
  }
}              

class BranchSelect extends React.Component {

  options() {
    return this.props.branches.map(b => <option value={b}>{b}</branch>);
  }

  render() {
    return(<select onChange={this.props.onselect}>
      {this.options.bind(this)}
      </select>);
  }
}

class DiffMergeRequest extends React.Component {

  render() {
    if (this.props.mergeRequest.targetBranch && this.props.mergeRequest.sourceBranch)

    else
      return ();
  }
}

class MergeRequest extends React.Component {
  constructor(props) {
    super(props);
  }
}

class BlobHistory extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], spinner: true };

    REST.readHistory(props.params.repo, 
                     props.params.branch, 
                     props.params.splat,
                     this.update.bind(this));
  }

// todo BlobHistory vs CommitList
  commit(e) {
    let ago = Time.ago(Time.parse(e.COMMITTER.TIME));
    return (
      <div>
      <hr />
      <table>
      <tr>
      <td>Description:</td>
      <td><Link to={this.props.params.repo + "/commit/" + e.SHA1}>{e.TEXT}</Link></td>
      </tr>
      <tr>
      <td>Time:</td>
      <td>{ago}</td>
      </tr>
      <tr>
      <td>Name:</td>
      <td><Link to={"/user/" + e.COMMITTER.NAME}>{e.COMMITTER.NAME}</Link></td>
      </tr>
      </table>
      </div>);
  }
      
  update(d) {
    this.setState({data: d, spinner: false});
  }
  
  render() {
    return(<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>History - {this.props.params.splat}</h1>
      {this.state.spinner?<Spinner />:this.state.data.map(this.commit.bind(this))} 
      </div>);
  }
} 

class Blob extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], spinner: true };
    REST.readBlob(props.params.repo, 
                  props.params.branch, 
                  props.params.splat,
                  this.update.bind(this));
  }
  
  determineLanguage() {
    if (/.(xml|html)$/i.test(this.props.params.splat)) {
      return "language-markup";
    } else if (/.(abap)$/i.test(this.props.params.splat)) {
      return "language-abap";
    } else {
      return "language-unknown";
    }      
  }
      
  update(d) {
    this.setState({data: d, spinner: false});
    Prism.highlightAll();
  }
      
  render() {
    let lang = this.determineLanguage();
    let history = this.props.params.repo + "/history/" + this.props.params.branch + "/" + this.props.params.splat;
    return(<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>{this.props.params.splat}</h1>
      <Link to={history}>File History</Link>
      {this.state.spinner?<Spinner />:<pre><code className={lang}>{this.state.data}</code></pre>}             
      </div>);
  }
}               

class Breadcrumb extends React.Component {
  bread;
  path;
           
  constructor(props) {
    super(props);
    this.build(props);
  }
      
  componentWillReceiveProps(props) {
    this.build(props);
  }         
    
  lookupParam(p) {
    if (p === "*") {
      return this.props.params["splat"];
    } else {
      return this.props.params[p.substr(1)];
    }
  }    
    
  route(e) {
    let name = "";
    if (e.bread) {
      name = e.bread;
      this.path = this.path + e.path;
      if (e.path !== "/") {
        this.path = this.path + "/";
      }
    } else if (e.path) {
      name = this.lookupParam(e.path)
      if(!name) {
        this.path = this.path + e.path + "/";
      } else {
        this.path = this.path + name + "/";
      }
    } else {
      return;
    }
    if (name) {
      this.bread.push({name, path: this.path});
    }
  }
  
  build(props) {
    this.bread = [];
    this.path = "";    
    
    props.routes.forEach(this.route.bind(this));
// remove link for last breadcrumb    
    this.bread[this.bread.length - 1] = {name: this.bread[this.bread.length - 1].name};
  }              

  show(e) {
    if (e.path) {
      return (<div className="inline">
              <Link className="inline" to={e.path}>{e.name}</Link>
              <div className="inline">&nbsp;&gt;&nbsp;</div>
              </div>);
    } else {
      return (<div className="inline">{e.name}</div>);
    }      
  }
    
  render() {
    document.title = "abapGitServer - " + this.bread[1].name;

    return(<table>
      <tr>
      <td><Link to="/"><img src={base + "/static/logo.svg"} height="50"></img></Link></td>
      <td>{this.bread.map(this.show)}</td>
      </tr>
      </table>);
  }           
}           
           
class RepoList extends React.Component {
  constructor() {
    super();
    this.state = {data: [], spinner: true};
    REST.listRepositories(this.update.bind(this));
  }
    
  update(d) {
    this.setState({data: d, spinner: false});
  }

  branch(e, name) {
    return (<div>
      {Octicons.branch()}&nbsp;{e.BRANCH.NAME}&nbsp;
      <Link to={name + "/files/" + e.BRANCH.NAME}>files</Link>&nbsp;
      <Link to={name + "/commits/" + e.BRANCH.NAME}>commits</Link>
      <br />
      Changed {Time.ago(Time.parse(e.LATEST.AUTHOR.TIME))}
      </div>);
  }

  repo(e) {
    return (
      <div className="tile">
      {Octicons.repo()} <Link to={e.REPO.NAME + "/"}>{e.REPO.NAME}</Link><br />
      <div className="italic inline">{e.REPO.DESCRIPTION}       
      &nbsp;
      <Link to={"/edit/" + e.REPO.NAME}>{Octicons.pencil()}</Link></div>
      <br /><br />
      {e.BRANCHES.map(function(b) { return this.branch(b, e.REPO.NAME); }, this)}
      </div>);
  }          
      
  render() {
    document.title = "abapGitServer";

    return (
      <div>

      <table>
      <tr><td>
      <a href="./"><img src={base + "/static/logo.svg"} height="100"></img></a>
      </td><td>
      <h1>abapGitServer</h1>
      </td></tr>
      </table>          
      {this.state.spinner?<Spinner />:this.state.data.map(this.repo.bind(this))}
      <br />
      {Octicons.plus()} <Link to="/create">Create</Link>
      <br />
      <br />
      {Octicons.clippy()} {cloneURL("<name>")}
      <br />
      <p className="right">
      <a href="https://github.com/larshp/abapGitServer">abapGitServer on GitHub</a>&nbsp;|&nbsp;
      <a href="/sap/zabapgitserver/rest/swagger.html">Swagger API</a>
      </p>
      </div>);
  }
}

class User extends React.Component {
  render() {
    return(<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>{this.props.params.user}</h1>
      todo, list commits by user
      </div>)
  }
}
      
class FilesList extends React.Component {
  constructor(props) {
    super(props);
    this.init(props);
  }

  init(props) {
    this.state = {data: [], spinner: true};
    this.path = props.params.splat?"/" + props.params.splat + "/":"/";
    REST.listFiles(props.params.repo, props.params.branch, this.path, this.update.bind(this));      
  }

  componentWillReceiveProps(props) {
    this.init(props);
  } 

  update(d) {
    this.setState({data: d, spinner: false});
  }
  
  file(e) {
    let url = "";
    if(e.CHMOD === "40000") {
      url = this.props.params.repo + "/files/" + this.props.params.branch + this.path + e.FILENAME;
    } else {
      url = this.props.params.repo + "/blob/" + this.props.params.branch + e.PATH + e.FILENAME;
    }
    url = url.replace(/#/g, "%23");
    let commit = this.props.params.repo + "/commit/" + e.LAST_COMMIT_SHA1;
    let icon = e.CHMOD === "40000"?Octicons.directory():Octicons.file();

    return (
      <tr>
      <td>{icon}</td>
      <td><Link to={url}>{e.FILENAME}</Link></td>
      <td><Link to={commit}>{e.COMMENT}</Link></td>
      <td>{Time.ago(Time.parse(e.TIME))}</td>
      </tr>);
  }
                           
  render() {
    let list = this.props.params.repo + "/commits/" + this.props.params.branch;

    return (
      <div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>{this.props.params.repo}</h1>
      {Octicons.commit()} <Link to={list}>list commits</Link><br />
      path: {this.path}<br />
      <br />
      <table>
      {this.state.spinner?<Spinner />:this.state.data.map(this.file.bind(this))}
      </table>
      </div>);
  }
}                  
      
class Router extends React.Component {
        
  render() { 
    const history = ReactRouter.useRouterHistory(History.createHistory)({ basename: base });
      
/*
* FRONTEND FOLDER OVERVIEW
*
* FOLDER                            COMPONENT       DESCRIPTION
* /                                 RepoList        list repositories
* /git/                             N/A             used for backend
* /static/                          N/A             used for backend
* /rest/                            N/A             used for backend
* /create/                          Create          create repository
* /edit/(name)                      Edit            edit repo description
* /user/(name)                      User            display user information/list commits
* /(name)/                          BranchList      list branches
* /(name)/commit/(sha1)             Commit          display commit
* /(name)/files/(branch)/(*)        FilesList       list files in branch 
* /(name)/commits/(branch)/         CommitList      list commits
* /(name)/blob/(branch)/(*)         Blob            display blob
* /(name)/history/(branch)/(*)      BlobHistory     blob history
*/

    return (
      <ReactRouter.Router history={history} >
        <ReactRouter.Route path="/" bread="abapGitServer">
          <ReactRouter.IndexRoute component={RepoList} />
          <ReactRouter.Route path="create" component={Create} bread="Create" />
          <ReactRouter.Route path="edit/:repo" component={Edit} bread="Edit" />
          <ReactRouter.Route path="user/:user" component={User} bread="User" />
          <ReactRouter.Route path=":repo">
            <ReactRouter.IndexRoute component={BranchList} />
            <ReactRouter.Route path="commit">
              <ReactRouter.Route path=":sha1" component={Commit}  />
            </ReactRouter.Route>
            <ReactRouter.Route path="files">
              <ReactRouter.IndexRoute component={NoMatch} />
              <ReactRouter.Route path=":branch">
                <ReactRouter.IndexRoute component={FilesList} />
                <ReactRouter.Route path="*" component={FilesList} />
              </ReactRouter.Route> 
            </ReactRouter.Route>
            <ReactRouter.Route path="commits">
              <ReactRouter.Route path=":branch" component={CommitList}  />
            </ReactRouter.Route>
            <ReactRouter.Route path="blob">
              <ReactRouter.Route path=":branch">
                <ReactRouter.IndexRoute component={NoMatch} />
                <ReactRouter.Route path="*" component={Blob} />
              </ReactRouter.Route>      
            </ReactRouter.Route>
            <ReactRouter.Route path="history">
              <ReactRouter.Route path=":branch">
                <ReactRouter.IndexRoute component={NoMatch} />
                <ReactRouter.Route path="*" component={BlobHistory} />
              </ReactRouter.Route>      
            </ReactRouter.Route>
          </ReactRouter.Route>
        </ReactRouter.Route>
        <ReactRouter.Route path="*" component={NoMatch} />
      </ReactRouter.Router>);
  }
}
      
ReactDOM.render(<Router />, document.getElementById('app'));
