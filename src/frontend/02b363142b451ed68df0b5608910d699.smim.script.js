const base = '/sap/zgit';
const Link = ReactRouter.Link;

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
    this.get("list/", callback);
  }

  static createRepository(data, callback) {
    this.post("create/", callback, data);
  }

  static listBranches(repoName, callback) {
    this.get("repo/" + repoName + "/branches", callback);    
  }
  
  static listFiles(repoName, callback) {
// todo, branch as input    
    this.get("repo/" + repoName + "/tree/master", callback);
  }

  static listCommits(repoName, branch, callback) {
    this.get("repo/" + repoName + "/commits/" + branch, callback);
  }

  static readBlob(repoName, branch, filename, callback) {
    const url = "repo/" + repoName + "/blob/master/" + filename;
    this.get(url, callback, false);
  }

  static readCommit(repoName, sha1, callback) {
    const url = "repo/" + repoName + "/commit/" + sha1;
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
}

class NoMatch extends React.Component {
  render() {
    return (<h1>router, no match</h1>);
  }
}

class CommitList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], spinner: true };
    REST.listCommits(props.params.repo, 
                     props.params.branch, 
                     (d) => { this.update(d);});      
  }

  update(d) {
    this.setState({data: d, spinner: false});
  }    

  commit(e) {
    let ago = Time.ago(Time.parse(e.COMMITTER.TIME));
    return (<div>
            <hr />
            Key: <Link to={this.props.params.repo + "/commit/" + e.SHA1}>{e.SHA1}</Link><br />
            Name: {e.COMMITTER.NAME}<br />
            Description: {e.TEXT}<br />
            Time: {ago}
            </div>);
  }
    
  render() {
    return (<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>Commits</h1>
      {this.state.spinner?<Spinner />:this.state.data.map(this.commit.bind(this))}
      </div>);
  }
}
            
class Spinner extends React.Component {
  render() {
    return(<div className="sk-rotating-plane"></div>);
  }
}  
           
class Create extends React.Component {
  constructor() {
    super();
    this.state = {name: "", description: ""};
  }
    
  callback(d) {
    alert("Done");
  }
    
  click(e) {
//    console.log(JSON.stringify(this.state));
    REST.createRepository(this.state, this.callback.bind(this));
    e.preventDefault();
  }           
         
  changeName(e) {
    this.setState({name: e.target.value, description: this.state.description});
  }
    
  changeDesc(e) {
    this.setState({name: this.state.name, description: e.target.value});
  }
    
  render() {
    return(
      <div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>Create</h1>
      <table border="1">
      <form>
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
      <td colspan="2">
      <input type="submit" value="Create" onClick={this.click.bind(this)}/>
      </td>
      </tr>
      </form>
      </table>
      </div>);
  }
}         

class Commit extends React.Component {
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
                    (d) => { this.update(d);}); 
  }      
      
  update(d) {
    this.setState({data: d, spinner: false});
  }      

  renderCommit() {
    return (<div>
            Name: {this.state.data.COMMITTER.NAME}<br />
            Description: {this.state.data.TEXT}<br />
            Parent: 
            <Link to={this.props.params.repo + "/commit/" + this.state.data.PARENT}>
            {this.state.data.PARENT}</Link><br />
            <br />
            todo, more information
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
    this.state = {data: [], spinner: true };
    REST.listBranches(props.params.repo, 
                      (d) => { this.update(d);});      
  }
  
  update(d) {
    this.setState({data: d, spinner: false});
  }
  
  single(e) {
    let cla = "";
    if (e.HEAD === "X") {
      console.log("bold");
      cla = "bold";
    }
    
    return (<tr>
      <td>
      <Link className={cla} to={this.props.params.repo + "/" + e.NAME}>
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
  
  render() {
    return(<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>Branch list</h1>
      {this.state.spinner?<Spinner />:this.list()}                      
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
                  (d) => { this.update(d);});      
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
    return(<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>{this.props.params.splat}</h1>
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
      this.path = this.path + name + "/";
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
    return(<div>{this.bread.map(this.show)}</div>);
  }           
}           
           
class RepoList extends React.Component {
  constructor() {
    super();
    this.state = {data: [], spinner: true};
    REST.listRepositories((d) => { this.update(d);});      
  }
    
  update(d) {
    this.setState({data: d, spinner: false});
  }

  repo(e) {
    return (
        <div>
          <Link to={e.NAME+"/"}>{e.NAME}</Link><br />
          {e.DESCRIPTION}<br />
          <br />
        </div>);
  }    
    
  render() {
    return (
      <div>
      <h1>abapGitServer</h1>
      {this.state.spinner?<Spinner />:this.state.data.map(this.repo)}
      <Link to="/create">Create new</Link>
      </div>);
  }
}
            
class FilesList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], spinner: true};
    REST.listFiles(props.params.repo, (d) => { this.update(d);});      
  }

  update(d) {
    this.setState({data: d, spinner: false});
  }
  
  file(e) {
    let url = this.props.params.repo + "/" + this.props.params.branch + "/blob" + e.FILENAME;
    let commit = this.props.params.repo + "/commit/" + e.COMMIT_SHA1;
    return (
      <tr>
      <td><Link to={url}>{e.FILENAME}</Link></td>
      <td><Link to={commit}>{e.COMMENT}</Link></td>
      <td>{Time.ago(Time.parse(e.TIME))}</td>
      </tr>);
  }
                           
  render() {
    let clone = window.location.origin + base + "/git/" + this.props.params.repo + ".git";
    let list = this.props.params.repo + "/" + this.props.params.branch + "/commits";
      
    return (
      <div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>{this.props.params.repo}</h1>
      Clone URL: {clone}<br />
      <Link to={list}>list commits</Link><br />
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
* FRONTEND folder overview 
*
* FOLDER                            COMPONENT       DESCRIPTION
* /                                 RepoList        list repositories
* /create/                                          create repository
* /edit/(name)                                      edit repo description
* /(name)/                          BranchList      list branches
* /(name)/(branch)/                 FilesList       list files in branch 
* /(name)/(branch)/commits          CommitList      list commits
* /(name)/(branch)/blob/(filename)  Blob            display blob
* /(name)/commit/(sha1)             Commit          display commit
*/

    return (
      <ReactRouter.Router history={history} >
        <ReactRouter.Route path="/" bread="abapGitServer">
          <ReactRouter.IndexRoute component={RepoList} />
          <ReactRouter.Route path="create" component={Create} bread="Create" />
          <ReactRouter.Route path=":repo">
            <ReactRouter.IndexRoute component={BranchList} />
            <ReactRouter.Route path="commit">
              <ReactRouter.Route path=":sha1" component={Commit}  />
            </ReactRouter.Route>
            <ReactRouter.Route path=":branch">
              <ReactRouter.IndexRoute component={FilesList} />
              <ReactRouter.Route path="commits" component={CommitList} bread="Commits" />
              <ReactRouter.Route path="blob">
                <ReactRouter.Route path="*" component={Blob} />
              </ReactRouter.Route>      
            </ReactRouter.Route>
          </ReactRouter.Route>
        </ReactRouter.Route>
        <ReactRouter.Route path="*" component={NoMatch} />
      </ReactRouter.Router>);
  }
}
      
ReactDOM.render(<Router />, document.getElementById('app'));