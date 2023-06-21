# App Downloader

This tool allows to download ranking and apps from both the Android (APK) and Apple (IPA) App Store.


## Installation


```
$> sbt stage
...
```

Then you need to install and configure [googleplay](https://github.com/googleplay-persistent) and the [ipa-tool](https://github.com/the-ok-is-not-enough/ipatool-py).

Finally adapt the configuration, the most important iOS keys are `ios.login` where you need to provide the `email` and `password` for your Apple account as well as the `twoFA` code send to your device during the ipa-tool configuration as well as `ios.ipatoolpy` which requires the path to the `main.py` of the installed ipa-tool.

For Android you need to configure `android.login` but only need to provide `email` and `password` and also set the correct path for `android.googleplay`.

You can ignore the `downloaderConfig` as this is only required for advanced features not involved for downloading data from the app store.

### Using the App-Downloader

Assuming a working configuration you can use the app downloader via the included script `app-downloader`, the process for android and apple is equal besides using the action keywords `android` and `ios`.

```
$>./app-downloader ios download full-chain
...
```

If you configured everything correctly it will now download the ranking first, and then the IPAs/APKs.