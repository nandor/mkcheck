// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

import React from 'react';
import TreeNode from '../components/TreeNode.jsx';


export default class TreeView extends React.Component {
  constructor(props) {
    super(props)
    this.state = { highlighted: null };
  }

  onSearch(ev) {
    const term = ev.target.value;
    const nodes = this.props.nodes;

    const selected = [];
    for (const key of Object.keys(nodes)) {
      const node = nodes[key];
      if (node.fileName().indexOf(term) != -1) {
        selected.push(key);
      }
    }

    const highlighted = new Set();
    for (const key of selected) {
      for (let node = nodes[key]; node; node = nodes[node.parent]) {
        highlighted.add(node.uid);
      }
    }

    if (highlighted.size < 50) {
      this.setState({ highlighted });
    } else {
      this.setState({ highlighted: null });
    }
  }

  render() {
    return (
      <div>
        <input type="text" onChange={(ev) => this.onSearch(ev)} />
        <TreeNode node={this.props.root} highlighted={this.state.highlighted} />
      </div>
    );
  }
};
