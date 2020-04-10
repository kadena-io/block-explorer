# Kadena Block Explorer

## Deployment Configs

To deploy the block explorer, use the deployment functionality from
[Obelisk](https://github.com/obsidiansystems/obelisk). Before deploying you may
want to create some or all of the following configs:

### Data Backends

In order to get block explorer features not provided by nodes, you have to
configure a data backend.  To do that, put something like the following in
`config/frontend/data-backends`:

```
{
  "mainnet01": { "hostAddress": "example.com", "hostPort": 80 }
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

