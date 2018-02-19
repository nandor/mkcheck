// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

class TreeNode {

}

export default class Tree {
  constructor(uid, parent, id, name, deleted, exists, deps, children) {
    this.uid = uid;
    this.parent = parent;
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
    let nextUID = 1;

    const buildNode = (parent, files, name, depth) => {
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

      const uid = nextUID++;

      let children = {};
      for (const key of Object.keys(byToken)) {
        children[key] = buildNode(
            uid,
            byToken[key],
            name + '/' + key,
            depth + 1
        );
      }

      const deleted = current ? current.deleted : false;
      const deps = current ? current.deps : [];
      const id = current ? current.id : 0;

      const exists = current ? current.exists : Object.keys(children).some(key => {
        return children[key].exists;
      });

      const node = new Tree(
          uid,
          parent,
          id,
          name || '/',
          deleted,
          exists,
          deps,
          children
      );

      if (id != 0) {
        byID[uid] = node;
      }
      return node;
    };

    return { root: buildNode(0, files, '', 0), nodes: byID };
  }
}
