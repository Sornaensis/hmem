# Local scratch work

Use the repository's top-level `.tmp/<task-or-purpose>/` for temporary scripts,
build experiments, browser profiles, screenshots and test diagnostics. `.tmp/`
is ignored by Git and excluded from Docker build contexts. Create it as needed;
Git does not preserve empty directories.

Give each active task one contained directory with an identified owner and a
finite lifetime. Remove its generated files after owned processes have stopped
and checks finish. Keep only bounded diagnostics for unresolved failures, and
remove those when the issue closes. Do not delete another task's active inputs
or registered worktrees.

Normal Stack, npm and Elm caches retain their tool defaults. Put experimental
build directories under the task's `.tmp/` directory where the tool supports it.
Do not retain routine successful test logs or copies of fixtures.

Existing versioned benchmark evidence and explicitly retained recovery or
release artifacts keep their recorded locations and retention requirements.
Historical manifests and commands describe their original paths; they are not
instructions to create new scratch outside `.tmp/`. Preserve retained artifacts
until their recorded owner disposes of them or an approved replacement exists.
