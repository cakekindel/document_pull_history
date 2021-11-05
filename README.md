# Script to back up PR history of a repo

## Running
This repo contains 2 scripts:
- `get_pulls` - Gets all pull requests (and associated discussions) from a repo
- `render_pulls` - Given the output of the first script, outputs the pulls rendered in a markdown file

### Requirements
- asdf

```sh
> asdf plugin add purescript
> asdf plugin add spago
> asdf install
```

### Running get_pulls
```sh
> cd get_pulls
> GH_TOKEN="<snip>" \
  GH_USERNAME="<snip>" \
  REPO="imercatus/imercata-backend" \
  OUT_FILE="./out.json" \
  spago run
```

### Running render_pulls
```sh
# TODO
```
