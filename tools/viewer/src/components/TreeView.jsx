// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

import React from 'react';



export default class TreeView extends React.Component {
  render() {
    const node = this.props.node;
    return (
      <div className="tree-view">
        <div className="tree-view-title">
          {node.name}
        </div>
        <div className="tree-view-children">
          {Object.keys(node.children).map((key, idx) => {
            return <TreeView key={idx} node={node.children[key]}/>;
          })}
        </div>
      </div>
    );
  }
};
