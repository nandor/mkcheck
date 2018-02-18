// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

class TreeNode {

}

export default class Tree {
  constructor(id, name, deleted, deps, children) {
    this.id = id;
    this.name = name;
    this.deleted = deleted;
    this.deps = deps;
    this.children = children;
  }

  fileName() {
    const tokens = this.name.split('/');
    return tokens[tokens.length - 1] || '/';
  }

  static build(data) {
    const files = data.map((file) => ({
      path: file.name.split('/').splice(1),
      file: file
    })).sort();

    const byID = {};

    const buildNode = (files, name, depth) => {
      const byToken = {};
      let current = null;
      files.forEach((file) => {
        if (file.path.length > depth) {
          const token = file.path[depth];
          byToken[token] = byToken[token] || [];
          byToken[token].push(file);
        } else if (file.path.length == depth) {
          current = file.file;
        }
      });

      let children = null;
      for (const key of Object.keys(byToken)) {
        children = children || {};
        children[key] = buildNode(byToken[key], name + '/' + key, depth + 1);
      }

      const deleted = current ? current.deleted : false;
      const deps = current ? current.deps : [];
      const id = current ? current.id : 0;

      const node = new Tree(id, name || '/', deleted, deps, children);
      if (id != 0) {
        byID[id] = node;
      }
      return node;
    };

    const root = buildNode(files, '', 0);
    return { root: root, nodes: byID };
  }
}
