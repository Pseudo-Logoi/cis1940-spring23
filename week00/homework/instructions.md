# Homework 0: Getting Started

The purpose of this assignment is to make sure you set up Haskell as soon as possible. As a result, we may ask you to do things that we won't explain in detail — just follow the steps and check that your programming environment responds as expected. Please come to office hours or post on Ed if you run into issues.

**Suggested Due Date**: Monday, Jan. 16 at 10 p.m.

It would be ideal if you could install Haskell before the first class, but we understand that configuring a new language can be tricky. You can submit this homework anytime before **Friday, Jan. 20** with no late penalty.

## Installation

The Git instructions are a strong suggestion. The Haskell instructions are _requirements_: we will expect you to use Stack and VSCode, and we will not help you if you encounter problems due to using a different setup.

### Step 0: Git

(This step is closely adapted from CIS 5710's instructions.)

The in-class exercises and homework assignments will be distributed via GitHub.

1.  If this is your first time using git, you should find and follow a tutorial online to learn the basic concepts. In the process, you should install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git), create a [github](https://github.com/join) account, and (optionally) setup an [SSH key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh).

    You should be familiar with commands like `add, commit, status, push, pull`, so that you can use version control effectively when completing the assignments.

2.  [Create](https://github.com/new) a new, **private** repo. Do **not** select any options under "repository template" or "initialize this repository with."

3.  Clone your repo:

    ```
    > git clone git@github.com:YOURUSERNAME/YOURREPO.git
    > cd YOURREPO
    ```

4.  Connect to our class repo:

    ```
    > git remote add upstream https://github.com/jwshi21/cis1940-spring23.git
    > git fetch upstream
    ```

5.  Merge the changes and push them back to GitHub:

    ```
    > git merge upstream/main
    > git push
    ```

6.  Whenever there are changes to the class repo, get those changes locally, too:

    ```
    > git fetch upstream
    > git merge upstream/main
    ```

    You will want to do this before each class and after each homework is released.

### Step 1: GHCup and Stack

(The remaining steps are closely adapted from CIS 5520's instructions.)

We will manage Haskell versions and packages using Stack.

1. Install [GHCup](https://www.haskell.org/ghcup/) by following the instructions on the homepage.

    Make sure to choose "yes" when asked about the Haskell Language Server and Stack.

2. At this point, you should be able to run Haskell programs! (You may need to restart the terminal for the installation to take effect.)

    Within the `week0/homework` directory, run this command to start the [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop):

    ```
    > stack ghci Exercises.hs
    ```

    Then, input `main` and press enter — this evaluates `main`. It should print out "You have installed Haskell!" There should also be an error message saying that a test failed; we will fix this in the last part.

### Step 2: VSCode

We will use Visual Studio Code as our editor.

1.  Install [VSCode](https://code.visualstudio.com/) by following the instructions on the homepage.

2.  Within VSCode, install the extensions named `Haskell` and `haskell-linter`.

3.  Open the command palette (`Ctrl/Cmd-Shift-P`) and go to "Preferences: Open User Settings (JSON)." Paste in these settings:

    ```
    {
    "editor.formatOnSave": true,
    "haskell.hlint.logLevel": "warn",
    "haskell.hlint.run": "onSave",
    "haskell.formattingProvider": "ormolu",
    "haskell.toolchain": {
        "hls": "1.9.0.0",
        "ghc": null,
        "cabal": null,
        "stack": null
    },
    "haskell.serverEnvironment": {
        "PATH": "${HOME}/.ghcup/bin:$PATH"
    },
    "haskell.manageHLS": "GHCup"
    }
    ```

4.  Open the `homework` folder for `week00` in VSCode. Make sure you have this folder open — not a parent directory or individual file.

5.  Go to `Exercises.hs` and complete Exercises 0, 1, and 2, which check that the compiler, autoformatter, and linter are working as expected, respectively.

6.  VSCode support for Haskell is excellent overall, but it can be finicky at times.

    If Exercise 0 is not working but you are not getting an error message from VSCode, this may mean the language server hasn't "woken up" yet. Click around a bit in `Exercises.hs` until `Processing` briefly appears in the blue bar at the bottom of the screen.

    Otherwise, problems often go away if you go to the command palette and run "Haskell: Restart Haskell LSP Server" or "Developer: Reload Window."

7.  If Exercise 2 does not work, try the following. Run

    ```
    > stack install hlint
    ```

    and take note of the message at the end saying where `hlint` was installed.

    Then add that path to the user settings above.

    ```
    "haskell.hlint.executablePath": "<path-to-hlint>"
    ```

    This should allow you to see the message in Exercise 2.

    However, it is likely that you will get an error when you try to _apply_ the hint. Tragically, there seems to be a [bug](https://github.com/haskell/haskell-language-server/issues/3241) at the moment. Two options:

    -   You can choose not to use this feature and instead apply hints manually — that is, replace the incorrectly styled code under the "Found:" heading with the correctly styled code under the "Why not:" heading.

    -   Or, you can follow this fix, which takes a while to run (since you are re-compiling the language server from scratch) but allows you to apply hints automatically:

        ```
        > cabal update
        > ghcup rm hls 1.9.0.0
        > ghcup compile hls --git-ref master --ghc 9.2.5
        ```

## Submission

Now that we have our basic setup, we will walk through the workflow of completing exercises, running tests, and submitting to Gradescope.

1. Start GHCi with

    ```
    > stack ghci Exercises.hs
    ```

    Run `main`. It should give the same message as before.

2. Go to `Exercises.hs` and complete Exercise 3.

3. In GHCi, input `:r` to reload the file. Run `main` again. The test should pass now.

4. Submit only `Exercises.hs` to Gradescope. It should pass the autograder test for Exercise 3. We will manually grade Exercises 1 and 2.
