class REST {
  listRepositories() {
// http://52.208.177.144:50000/sap/zgit/rest/repositories/    
  }
}

class NoMatch extends React.Component {
  render() {
    return (<h1>router, no match {this.props.params.id}</h1>);
  }
}
            
class RepoList extends React.Component {
  render() {
    return (
      <div><h1>RepoList</h1>
      <ReactRouter.Link to="/sap/zgit/foobar/">foobar</ReactRouter.Link>
      </div>);
  }
}
            
class Repo extends React.Component {
  render() {
    return (
      <div>
      <h1>Repo {this.props.params.id}</h1>
      file list
      </div>);
  }
}                  
            
class Router extends React.Component {
  render() {
      
    return (
      <ReactRouter.Router history={ReactRouter.browserHistory} >
        <ReactRouter.Route path="/">
          <ReactRouter.Route path="sap">
            <ReactRouter.Route path="zgit">
              <ReactRouter.IndexRoute component={RepoList} />
              <ReactRouter.Route path=":id" component={Repo} />
            </ReactRouter.Route>
          </ReactRouter.Route>
        </ReactRouter.Route>
        <ReactRouter.Route path=":path" component={NoMatch} />
      </ReactRouter.Router>);
  }
}

ReactDOM.render(<Router />, document.getElementById('app'));