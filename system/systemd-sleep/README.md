# systemd-sleep hooks

System-level hooks that run around suspend/hibernate transitions.

## restart-wifi

### Problem

On this laptop the Qualcomm WiFi (`ath11k_pci` driver) frequently comes back
broken after resuming from hibernate — the interface either fails to associate
or vanishes entirely. Reloading the kernel module restores it:

```sh
sudo modprobe -r ath11k_pci
sudo modprobe ath11k_pci
```

The `restart-wifi` script automates this by hooking into systemd-sleep so the
reload happens automatically on every resume from hibernate.

### Install

From the dotfiles repo root:

```sh
make system
```

That target symlinks the script into `/lib/systemd/system-sleep/`, so edits in
the repo take effect immediately without re-running `make`. Equivalent manual
command:

```sh
sudo ln -sf "$(pwd)/system/systemd-sleep/restart-wifi" \
    /lib/systemd/system-sleep/restart-wifi
```

systemd auto-discovers every executable under `/lib/systemd/system-sleep/` and
runs them around sleep transitions — there is **no `systemctl enable` step**.

### Verify it ran

After a resume:

```sh
journalctl -t restart-wifi
```

### Test without rebooting

Invoke the script manually with the same args systemd would pass:

```sh
sudo /lib/systemd/system-sleep/restart-wifi post hibernate
```

This will trigger the actual module reload, so expect WiFi to drop briefly.

### Uninstall

```sh
sudo rm /lib/systemd/system-sleep/restart-wifi
```

### Notes

- The hook only acts on `post/hibernate` and `post/suspend-then-hibernate`.
  Plain `post/suspend` is intentionally skipped because suspend doesn't
  exhibit the bug on this machine.
- If the same bug appears after plain suspend, add `post/suspend` to the
  `case` statement in `restart-wifi`.
