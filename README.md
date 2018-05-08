# TodoTxt

This is a simple command line application to handle [TodoTxt](http://todotxt.org/) files. More information about the TodoTxt file format can be found [here](https://github.com/todotxt/todo.txt). You can also [see a sample file](http://todotxt.org/todo.txt).

This application is at an early stage of development. Use it at your own risk.

## Installation

This application is designed for Linux and macOS. (I choose not to support Windows because this kind of tool is not much in the MS philosophy, moreover Windows has a poor support for command line: no UTF-8 or ANSI escape sequences by default.)

As this application is not officially released yet, there is no installer. It is a regular Haskell project based on Stack and Cabal. To install, clone the repository, build with `stack build` and copy the executable file to `/usr/local/bin/todo`.

The todo file is by default `~/todo.txt`. If this file does not exist, it is created the first time you add a task. You can change this default location by setting the `TODOTXT` environment variable to a valid path. For example, if you want to share your todo list among several computers, you can move the default location to your dropbox (or [nubo](https://github.com/PascalLG/Nubo) 😉) by adding the following line to your `.bashrc`:

```
export TODOTXT=~/Dropbox/Todo.txt
```

There is no support for multiple todo files.

## Usage

To familiarise yourself with this application usage, you can start with an existing todo file, such as [this one](http://todotxt.org/todo.txt). Just download it and save it as `~/todo.txt` before trying commands.

### Listing tasks

To list the tasks in your todo list, type:

```
todo ls
```

The first column displays the task rank (actually the line number in the todo file). This number is used to refer to tasks in other commands. The following columns display the priority (if any) as a letter from A to Z, the creation date, and the task description. The list is sorted by decreasing priority. Tasks having no priority come at the end of the list. Although the file format allows for a completion date, it is currently ignored.

Finished tasks are not listed by default. To list them as well, add the `--done` flag:

```
todo ls --done
```

You can filter tasks by adding keywords (or tags, see below) after the command. Only tasks containing *all* the specified keywords and tags are listed. (Comparison is not case-senstive.) For example:

```
todo ls @phone mom
```

### Adding tasks

To add a task, type:

```
todo add <task description>
```

Do not forget quotes around your description to prevent `bash` interpreting spaces and special characters. You can also provide several task in the same commands. For example:

```
todo add "Reply to John's mail"
todo add "Buy some potatoes" "Call Margaret"
```

A task description can contain tags. A *project* tag starts with a plus sign (+). It indicates the project this task belongs to. *context* tag starts with an at sign (@). It indicates the context (or the type) of a task. For example:

```
todo add "Create macOS and Debian packages for +TodoTxt @dev"
todo add "Call Margaret @phone"
```

You can use as many tags as you wish and they can appear anywhere in a task description. In task listings, tags are colourised to help identify them. Of course, a tag is just a convention, it is not interpreted by the application. You can assign a different meaning to + and @ tags if you wish.

### Prioritising tasks

The priority is represented by a letter from A to Z, A indicating the most important tasks and Z the less important ones. To assign a task a priority, type:

```
todo pri <rank> <priority>
```

The rank is the rank number returned by the `ls` command while the priority is a single capital letter. On the other hand, to remove a task priority, type:

```
todo depri <rank>
```

For example:

```
todo pri 12 A
todo depri 7
```

Prioritised tasks appear first in listings.

### Marking tasks as done

To mark a task as done, type:

```
todo done <rank>
```

A done task no longer appears in listing (unless the `--done` flag is specified) but it is still present in the todo list. Alternatively, to completely remove a task, add the `--remove` flag:

```
todo done --remove <rank>
```

If you change your mind and want to mark a task as not done, type:

```
todo undone <rank>
```

Of course, this is only possible if the task was not previously removed.

For example:

```
todo done 3
todo undone 3
todo done --remove 17
```

### Getting help

Calling `todo` with no arguments prints the list of available command. To get help on a specific command, type `todo help <command>`.
    