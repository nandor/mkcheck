// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

import React from 'react';



export default class TreeView extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      expanded: false
    };
  }
  render() {
    const node = this.props.node;
    return (
      <div className="tree-view">
        <div className="tree-view-title">
          { node.children ? (
            <a onClick={() => this.setState({expanded: !this.state.expanded})}>
              {this.state.expanded ? '-' : '+'}
              {node.fileName()}
            </a>
          ) : (
            node.fileName()
          )}
        </div>
        { this.state.expanded ? (
          <div className="tree-view-children">
            { node.children ? Object.keys(node.children).map((key, idx) => {
              return <TreeView key={idx} node={node.children[key]}/>;
            }) : (
              null
            )}
          </div>
        ) : (
          null
        )}
      </div>
    );
  }
};
