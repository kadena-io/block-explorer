# Kadena Block Explorer

## Deployment Configs

To deploy the block explorer, use the deployment functionality from
[Obelisk](https://github.com/obsidiansystems/obelisk). Before deploying you may
want to create some or all of the following configs:

### Google Analytics

Put the Google Analytics tracking ID in `config/frontend/tracking-id`. If you
don't want to enable the tracking ID, create an empty file with the following:

```
echo no-tracking > config/frontend/tracking-id
```
