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
    const children = Object.values(node.children).sort((a, b) => {
      const la = Object.keys(a.children).length;
      const lb = Object.keys(b.children).length;
      if ((la == 0 && lb == 0) || (la > 0 && lb > 0)) {
        if (a.name > b.name) {
          return 1;
        } else if (a.name < b.name) {
          return -1;
        } else {
          return 0;
        }
      }

      if (la > 0 && lb == 0) {
        return -1;
      } else {
        return 1;
      }
    });

    const colour = node.deleted ? 'red' : (!node.exists ? 'orange' : 'auto');

    return (
      <div className="tree-view">
        <div
            className="tree-view-title"
            style={{ color: colour }}>
          { children.length > 0 ? (
            <a onClick={() => this.setState({expanded: !this.state.expanded})}>
              {this.state.expanded ? '-' : '+'}
              {node.fileName()}
            </a>
          ) : (
            node.fileName()
          )}
        </div>
        { this.state.expanded && children.length > 0 ? (
          <div className="tree-view-children">
            { node.children ? children.map((child, idx) => {
              return <TreeView key={idx} node={child}/>;
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
