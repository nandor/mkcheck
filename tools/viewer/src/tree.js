// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

class TreeNode {

}

export default class Tree {
  constructor(id, name, deleted, exists, deps, children) {
    this.id = id;
    this.name = name;
    this.deleted = deleted;
    this.exists = exists;
    this.deps = deps;
    this.children = children;
  }

  fileName() {
    const tokens = this.name.split('/');
    return tokens[tokens.length - 1] || '/';
  }

  static build(data) {
    const files = data
      .map((file) => ({
        path: file.name.split('/').splice(1),
        file: file
      }))
      .filter(file => file.file.name != '/')
      .sort();

    const byID = {};

    const buildNode = (files, name, depth) => {
      const byToken = {};
      let c = null;
      files.forEach((file) => {
        if (file.path.length > depth) {
          const token = file.path[depth];
          byToken[token] = byToken[token] || [];
          byToken[token].push(file);
        } else if (file.path.length == depth) {
          c = file.file;
        }
      });

      let children = {};
      for (const key of Object.keys(byToken)) {
        children[key] = buildNode(byToken[key], name + '/' + key, depth + 1);
      }

      const deleted = c ? c.deleted : false;
      const deps = c ? c.deps : [];
      const id = c ? c.id : 0;
      const exists = c ? c.exists : Object.keys(children).some(key => {
        return children[key].exists;
      });

      const node = new Tree(id, name || '/', deleted, exists, deps, children);
      if (id != 0) {
        byID[id] = node;
      }
      return node;
    };

    const root = buildNode(files, '', 0);
    return { root: root, nodes: byID };
  }
}
