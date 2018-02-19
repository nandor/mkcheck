// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

import React from 'react';

function sortNode(a, b) {
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
}

export default class TreeNode extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      expanded: false
    };
  }

  render() {
    const node = this.props.node;

    const children = Object.values(node.children).sort(sortNode);

    const highlighted = (
        this.props.highlighted &&
        this.props.highlighted.has(node.uid)
    );

    const visible = children.length > 0 && (
        node.parent == 0 ||
        this.state.expanded ||
        highlighted
    );

    const colour =
        highlighted ? 'green' :
        node.deleted ? 'red' :
        !node.exists ? 'orange' :
        'black';

    return (
      <div className="tree-node">
        <div
            className="tree-node-title"
            style={{ color: colour }}>
          { children.length > 0 ? (
            <a onClick={() => this.setState({expanded: !this.state.expanded})}>
              {visible ? '-' : '+'}
              {node.fileName()}
            </a>
          ) : (
            node.fileName()
          )}
        </div>
        { visible ? (
          <div className="tree-node-children">
            { node.children ? children.map((child, idx) => {
              return <TreeNode
                  key={idx}
                  node={child}
                  highlighted={this.props.highlighted}
              />;
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
