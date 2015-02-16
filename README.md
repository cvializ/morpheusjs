morpheusjs
==========

Write JavaScript, compile it to the MOHAA "morpheus" scripting language.

Synopsis
--------

Medal of Honor: Allied Assault provides a scripting engine called morpheus. This engine allows developers to add interactivity to their custom maps. However, it lacks many features and tooling that support developers, such as debugging tools, testing frameworks, etc. This project attempts to resolve these gaps by compiling JavaScript to morpheus, bringing the vast js ecosystem of tools and libraries to this pseudo-serverside scripting environment.

Project Features
----------------

- A parser for morpheus script files to verify correct output
- A transpiler(?) to convert JavaScript to morpheus
- A test suite to mock the MOH game environment to allow unit testing of morpheusjs script projects
