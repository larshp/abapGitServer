const Link = ReactRouter.Link;

function handleError(evt, callback) {
  if (evt.target.status === 200) {
    callback(JSON.parse(evt.target.responseText).DATA);
  } else {
    alert("REST call failed, status: " + evt.target.status);
  }
}

class REST {
  static root = "/sap/zgit/rest/";

  static listRepositories(callback) {
    this.get("repositories/", callback);
  }

  static listFiles(repoName, callback) {
    this.get("repo/" + repoName + "/files/", callback);
  }

  static get(folder, callback) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback); });
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
      {this.state.data.map((e) => { return (<div><Link to={e.NAME+"/"}>{e.NAME}</Link><br /></div>);})}
      </div>);
  }
}
            
class Repo extends React.Component {
  constructor(props) {
    super(props);
    this.state = {data: [], spinner: true};
    REST.listFiles(props.params.id, (d) => { this.update(d);});      
  }

  update(d) {
    this.setState({data: d, spinner: false});
  }                                   
                                   
  render() {
    return (
      <div>
      <h1>{this.props.params.id}</h1>
      {this.state.spinner?<Spinner />:""}
      {this.state.data.map((e) => { return (<div>{e.FILENAME}<br /></div>);})}
      </div>);
  }
}                  
      
class Router extends React.Component {
        
  render() { 
    const history = ReactRouter.useRouterHistory(History.createHistory)({
      basename: '/sap/zgit'
      });
      
/*
* FRONTEND folder overview 
*
* Folder                 Component Description
* /                      RepoList  list repositories
* /create/                         create repository
* /repo/foobar/          Repo      list files
* /repo/foobar/edit                edit repo description
* /repo/foobar/blob/sha1           display blob
*/
    return (
      <ReactRouter.Router history={history} >
        <ReactRouter.Route path="/">
          <ReactRouter.IndexRoute component={RepoList} />
          <ReactRouter.Route path=":id" component={Repo} />
        </ReactRouter.Route>
        <ReactRouter.Route path="*" component={NoMatch} />
      </ReactRouter.Router>);
  }
}
      
ReactDOM.render(<Router />, document.getElementById('app'));