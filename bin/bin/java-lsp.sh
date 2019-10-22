#!/usr/bin/env bash

JAR="$HOME/dev/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_*.jar"
GRADLE_HOME=$HOME/gradle /usr/lib/jvm/java-13-openjdk/bin/java \
  -Declipse.application=org.eclipse.jdt.ls.core.id1 \
  -Dosgi.bundles.defaultStartLevel=4 \
  -Declipse.product=org.eclipse.jdt.ls.core.product \
  -Dlog.protocol=true \
  -Dlog.level=ALL \
  -noverify \
  -Xmx1G \
  -jar $(echo "$JAR") \
  -configuration "$HOME/dev/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/config_linux" \
  -data "$HOME/.local/share/eclipse" \
  --add-modules=ALL-SYSTEM \
  --add-opens java.base/java.util=ALL-UNNAMED \
  --add-opens java.base/java.lang=ALL-UNNAMED
