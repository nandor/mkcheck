// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

import React from 'react';
import {render} from 'react-dom';

import TreeView from './components/TreeView.jsx';
import Tree from './tree.js';

class App extends React.Component {
  constructor() {
    super();

    this.state = { tree: null };

    fetch('/data/files')
      .then(data => data.json())
      .then(data => {
        this.setState({ tree: Tree.build(data) });
      });
  }

  render () {
    if (!this.state.tree) {
      return null;
    }
    let { root, nodes } = this.state.tree;
    return <TreeView nodes={nodes} root={root}/>;
  }
}

render(<App/>, document.getElementById('app'));
