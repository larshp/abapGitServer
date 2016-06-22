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

class REST {
  static root = base + "/rest/";

  static listRepositories(callback) {
    this.get("list/", callback);
  }

  static listFiles(repoName, callback) {
    this.get("repo/" + repoName + "/tree/master", callback);
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

class Spinner extends React.Component {
  render() {
    return(<div className="sk-rotating-plane"></div>);
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
      <h1>Blob {this.props.params.splat}</h1>
      {this.props.params.branch}<br />
      <br />
      {this.state.spinner?<Spinner />:<pre><code className={lang}>{this.state.data}</code></pre>}             
      </div>);
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
            
  render() {     
    return (
      <div>
      <h1>abapGitServer</h1>
      {this.state.spinner?<Spinner />:""}
      {this.state.data.map((e) => { return (
        <div>
          <Link to={e.NAME+"/"}>{e.NAME}</Link><br />
          {e.DESCRIPTION}<br />
          <br />
        </div>);})}
      </div>);
  }
}
            
class Repo extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], spinner: true};
    REST.listFiles(props.params.repo, (d) => { this.update(d);});      
  }

  update(d) {
    this.setState({data: d, spinner: false});
  }                                   
                                   
  render() {
    return (
      <div>
      <h1>{this.props.params.repo}</h1>
      Clone URL: {window.location.origin}{base}/git/{this.props.params.repo}.git<br />
      <br />
      {this.state.spinner?<Spinner />:""}
      {this.state.data.map((e) => { return (
        <div>
        <Link to={this.props.params.repo + "/blob/master"+e.FILENAME}>{e.FILENAME}</Link>
        <br />
        </div>);})}
      </div>);
  }
}                  
      
class Router extends React.Component {
        
  render() { 
    const history = ReactRouter.useRouterHistory(History.createHistory)({
      basename: base
      });
      
/*
* FRONTEND folder overview 
*
* Folder                 Component       Description
* /                      RepoList        list repositories
* /create/                               create repository
* /repo/(name)/          Repo            list files in master/HEAD branch
* /repo/(name)/edit                      edit repo description
* /repo/(name)/blob/(branch)/(filename)  display blob
*/
    return (
      <ReactRouter.Router history={history} >
        <ReactRouter.Route path="/">
          <ReactRouter.IndexRoute component={RepoList} />
          <ReactRouter.Route path=":repo">
            <ReactRouter.IndexRoute component={Repo} />
            <ReactRouter.Route path="blob">
              <ReactRouter.Route path=":branch/*" component={Blob} />
            </ReactRouter.Route>
          </ReactRouter.Route>
        </ReactRouter.Route>
        <ReactRouter.Route path="*" component={NoMatch} />
      </ReactRouter.Router>);
  }
}
      
ReactDOM.render(<Router />, document.getElementById('app'));