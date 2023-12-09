# Shrinker

This tool helps shrinking the ABAP objects of one tool to a minimal number, like e.g. grouping several class pools into local classes in one program, converting ABAP Dictionary objects (data elements, structures, table types) into ABAP local types, etc.
Optionally, it can be used without shrinking the number of objects, but to copy and rename the objects.

Shrinker contains a copy of [abapGit](https://github.com/abapgit/abapgit) to create the shrinked ABAP objects.

Shrinker can be used in these situations for instance:
- You are distributing an application which is using a tool, that you prefer to be a copy of the original tool, so that your application doesn't depend on the version of the tool installed by the client.
- You are distributing an application which is installed by abapGit, you want to reduce the number of objects installed so that the installation is simple and fast.
- You have a tool used in many of your applications, you want to try the latest version of the tool with one application, but keep the old version of the tool for all other applications.
- You have several versions of a tool in your system and you want to limit the number of objects for some reason (e.g. to avoid the object search to propose many objects).

It proposes three demonstrations:
- The program `zshrinker_demo_abap2xlsx` can shrink [abap2xlsx](https://github.com/abap2xlsx/abap2xlsx) from 260 objects to 6 objects (could be 1 with more efforts), the program `zshrinker_demo_abap2xlsx_use` is using this shrinked version to generate an Excel file.
- The program `zshrinker_shrink_abapgit` can shrink [abapGit](https://github.com/abapgit/abapgit) from 494 objects to 2 includes and 1 program (which is equivalent to [`zabapgit_standalone`](https://raw.githubusercontent.com/abapGit/build/main/zabapgit_standalone.prog.abap). Could be 1 program by merging the two includes.
- The program `zshrinker_demo_shrinker` copy and rename the objects of Shrinker itself.
