#!/run/current-system/profile/bin/nu

cd /opt/ultra/jetbrains

let flags = [
    "--container"
    "--emulate-fhs"
    "--network"
    "--no-cwd"
]

let shared = [
    "/opt/ultra/jetbrains"
    "/opt/ultra/jetbrains-home=/home/kivilaak"
    "/villa/kivilaak/Code/UT=/home/kivilaak/Code/UT"
    "/villa/kivilaak/.ssh=/home/kivilaak/.ssh"
    "/run/user/1001"
    "/dev/shm"
]

let exposed = [
    "/tmp/.X11-unix"
    "/etc/machine-id"
    "/run/udev"
    "/var/run/dbus"
    "/dev/dri"
    "/sys/class/drm"
    "/sys/dev/char"
    "/sys/devices"
    "/proc/sys"
    "/proc/mounts"
    "/home/kivilaak/.gitconfig"
]

let preserved = [
    "^DISPLAY$"
    "^XAUTHORITY$"
    "^WAYLAND_DISPLAY$"
    "^XDG_RUNTIME_DIR$"
    "^XDG_SESSION_TYPE$"
    "^DBUS_"
    "^SSH_AUTH_SOCK$"
]

let packages = [
    gcc-toolchain coreutils zlib
    libx11 libxext libxrender libxtst libxi libxrandr libxcb libxfixes
    libsecret dbus freetype fontconfig harfbuzz libpng
    mesa mesa-utils libdrm xcb-util-keysyms xdg-utils tar gzip xz eudev qutebrowser
    nushell bash wayland libxkbcommon go gopls "openjdk@21:jdk" git node
    e2fsprogs util-linux nss-certs strace procps grep openssh which
]

let args = [
    ...$flags
    ...($shared    | each { |s| $"--share=($s)" })
    ...($exposed   | each { |e| $"--expose=($e)" })
    ...($preserved | each { |p| $"--preserve=($p)" })
    ...$packages
    "--" nu
]

^guix shell ...$args
