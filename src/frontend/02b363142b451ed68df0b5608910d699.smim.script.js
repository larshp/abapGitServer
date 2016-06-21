const Link = ReactRouter.Link;

class REST {
  listRepositories() {
// http://ip:50000/sap/zgit/rest/repositories/    
  }
}

class NoMatch extends React.Component {
  render() {
    return (<h1>router, no match</h1>);
  }
}
            
class RepoList extends React.Component {
  render() {
    return (
      <div><h1>RepoList</h1>
      <Link to="foobar/">foobar</Link>
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
    const history = ReactRouter.useRouterHistory(History.createHistory)({
      basename: '/sap/zgit'
      });
      
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