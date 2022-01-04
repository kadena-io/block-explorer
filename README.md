# Kadena Block Explorer

## Deployment Configs

To deploy the block explorer, use the deployment functionality from
[Obelisk](https://github.com/obsidiansystems/obelisk). Alternatively, you can
build locally with `nix-build -A exe`, run the `makeLinks` script to create
appropriate symlinks, and then run with `result/backend`. Before deploying you
may want to create some or all of the following configs:

### Data Backends

The block explorer gets blockchain data from a node's p2p and service APIs as
well as optionally getting more data from a chainweb-data server. This needs to
be configured something like the following in `config/frontend/data-backends`:

```
{
  "mainnet01": {
    "p2p": "https://node.example.com:443",
    "service": "http://node.example.com:1848",
    "data": "http://chainweb-data.example.com:8000"
  },
  "testnet04": {
    "p2p": "https://testnet-node.example.com:443",
    "service": "http://testnet-node.example.com:1848",
    "data": "http://testnet-chainweb-data.example.com:8000"
  }
}
```

This object will be used to look up the server given the chainwebVersion that
is being used by the node that the explorer is talking to.


### Google Analytics

Put the Google Analytics tracking ID in `config/frontend/tracking-id`. If you
don't want to enable the tracking ID, create an empty file with the following:

```
echo no-tracking > config/frontend/tracking-id
```

