# Moving Ollama data from root to home partition

Ollama installs its runtime libraries and model data on the root partition by
default. On a system where `/home` is a separate, larger partition, moving them
there recovers significant space.

---

## What takes space

| Path | Contents | Typical size |
|------|----------|-------------|
| `/usr/local/lib/ollama` | CUDA/GPU runtime libraries | ~7 GB |
| `/usr/share/ollama` | Service home: models, config, keys | ~3 GB |

`/usr/share/ollama` is the home directory of the `ollama` system user. It
contains `config.json`, SSH keypair, and downloaded models. It is separate from
`~/.ollama`, which is the current user's Ollama directory (created when running
`ollama` commands as a regular user).

---

## Steps

```bash
# 1. Stop the service
sudo systemctl stop ollama

# 2. Move system home and symlink back
sudo mv /usr/share/ollama $HOME/ollama
sudo ln -s $HOME/ollama /usr/share/ollama
sudo chown -R ollama:ollama $HOME/ollama

# 3. Move runtime libraries and symlink back
sudo mv /usr/local/lib/ollama $HOME/ollama/lib
sudo ln -s $HOME/ollama/lib /usr/local/lib/ollama

# 4. Tell the service where models live
sudo mkdir -p /etc/systemd/system/ollama.service.d
sudo tee /etc/systemd/system/ollama.service.d/override.conf <<'EOF'
[Service]
Environment="OLLAMA_MODELS=$HOME/ollama/models"
EOF

# 5. Reload and restart
sudo systemctl daemon-reload
sudo systemctl start ollama

# 6. Verify
systemctl status ollama
df -h /
```

---

## Notes

- The symlinks at `/usr/share/ollama` and `/usr/local/lib/ollama` keep existing
  paths working. No binaries or scripts need updating.
- Do not move `/usr/share/ollama` to `~/.ollama`. The system service runs as
  the `ollama` user, not as the login user. Mixing their data causes permission
  problems.
- If the `ollama` user's home is set to `/usr/share/ollama` in `/etc/passwd`,
  the symlink is sufficient — the service follows it.
- Future model downloads go to `$HOME/ollama/models` because of the
  `OLLAMA_MODELS` override.
