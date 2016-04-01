VR Pal

Bindings to Valve's OpenVR/SteamVR SDK and Oculus's SDK.

By default, only the SteamVR support is built in to minimize DLL dependencies (since SteamVR is meant to abstract over the Oculus SDK already).

To enable direct Oculus SDK support, use the flags `UseOculusSDK`.

With `stack` you can do this in your project's `stack.yaml` like so:
```yaml
flags:
  vr-pal:
    UseOculusSDK: true
```
