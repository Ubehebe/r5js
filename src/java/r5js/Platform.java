package r5js;

import com.google.common.collect.ImmutableList;

/**
 * A platform represents the JavaScript environment in which a specific {@link Target}
 * runs.
 *
 * <p>Platforms have platform-specific capabilities, which are made known to the compiler
 * through extern files.
 */
interface Platform {
    ImmutableList<String> externs();

    static final class Android implements Platform {
        @Override
        public ImmutableList<String> externs() {
            return ImmutableList.of("custom-externs/android.js");
        }
    }

    static final class Html5 implements Platform {
        @Override
        public ImmutableList<String> externs() {
            return ImmutableList.of(/* nothing special */);
        }
    }

    static final class Nashorn implements Platform {
        @Override
        public ImmutableList<String> externs() {
            return ImmutableList.of(/* nothing special */);
        }
    }

    static final class Node implements Platform {
        @Override
        public ImmutableList<String> externs() {
            return ImmutableList.of("externs/process.js");
        }
    }
}
