const base = '/sap/zgit';
const Link = ReactRouter.Link;

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

class Time {
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

class REST {
  static root = base + "/rest/";

  static listRepositories(callback) {
    this.get("list/", callback);
  }

  static listFiles(repoName, callback) {
    this.get("repo/" + repoName + "/tree/master", callback);
  }

  static listCommits(repoName, branch, callback) {
    this.get("repo/" + repoName + "/commits/" + branch, callback);
  }

  static readBlob(repoName, branch, filename, callback) {
    const url = "repo/" + repoName + "/blob/master/" + filename;
    this.get(url, callback, false);
  }

  static get(folder, callback, json = true) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback, json); });
    oReq.open("GET", this.root + folder);
    oReq.send();
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
    let date = new Date(parseInt(e.COMMITTER.substr(e.COMMITTER.indexOf(">") + 1)) * 1000);
    let ago = Time.ago(date);
    return (<div>{e.SHA1} {e.COMMITTER} {e.BODY} {ago}</div>);
  }
    
  render() {
    return (<div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>commit list</h1>
      {this.props.params.repo}<br />
      {this.props.params.branch}<br />
      <br />
      {this.state.spinner?<Spinner />:this.state.data.map(this.commit)}
      </div>);
  }
}
            
class Spinner extends React.Component {
  render() {
    return(<div className="sk-rotating-plane"></div>);
  }
}  
           
class Create extends React.Component {
  render() {
    return(<div>create</div>);
  }
}         
           
class BranchList extends React.Component {
  render() {
    return(<div>
           <Breadcrumb routes={this.props.routes} params={this.props.params} />
           <h1>Branch list</h1>
           <Link to={this.props.params.repo + "/master"}>master</Link>
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
  bread = [];
  path = "";
           
  constructor(props) {
    super(props);
    this.build();      
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
    if (e.name) {
      name = e.name;
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
  
  build() {
    this.props.routes.forEach(this.route.bind(this));
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
    return (
      <div>
      <Link to={this.props.params.repo + "/" + this.props.params.branch + "/blob" + e.FILENAME}>{e.FILENAME}</Link>
      <br />
      </div>);
  }
                           
  render() {
    return (
      <div>
      <Breadcrumb routes={this.props.routes} params={this.props.params} />
      <h1>{this.props.params.repo}</h1>
      Clone URL: {window.location.origin}{base}/git/{this.props.params.repo}.git<br />
      <Link to={this.props.params.repo + "/" + this.props.params.branch + "/commits"}>list commits</Link><br />
      <br />
      {this.state.spinner?<Spinner />:this.state.data.map(this.file.bind(this))}
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
*/

    return (
      <ReactRouter.Router history={history} >
        <ReactRouter.Route path="/" name="abapGitServer">
          <ReactRouter.IndexRoute component={RepoList} />
          <ReactRouter.Route path="create" component={Create} />
          <ReactRouter.Route path=":repo">
            <ReactRouter.IndexRoute component={BranchList} />
            <ReactRouter.Route path=":branch">
              <ReactRouter.IndexRoute component={FilesList} />
              <ReactRouter.Route path="commits" component={CommitList} name="Commits" />
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