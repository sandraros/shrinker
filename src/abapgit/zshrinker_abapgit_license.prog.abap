********************************************************************************
*
* LICENSE and NOTICE
*
* Shrinker is using an adapted copy of abapGit named "Shrinked abapGit",
* whose objects were generated automatically by Shrinker from the abapGit
* original objects, as described right after the abapGit license below.
*
* abapGit is licensed under the MIT License described below.
*
********************************************************************************


********************************************************************************
*
* The MIT License (MIT)
*
* Copyright (c) 2014 abapGit Contributors
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*
********************************************************************************


********************************************************************************
*
* This version of "Shrinked abapGit" has been made as follows:
*
*   1. Log in system ABAP 7.52 SP 04
*   2. Install abapGit to any package e.g. $abapgit
*      - Version of abapGit used: https://github.com/abapGit/abapGit/commit/6b8b29cd46a75a654ad94a5947ed528c980b09c9
*   3. Install Shrinker via abapGit to any package e.g. $shrinker
*      - Version of Shrinker used: https://github.com/sandraros/shrinker/commit/ae365d0bff060104c48c3fa43cf06f6cdf3eb78b
*   4. Run the program ZSHRINKER_DEMO_SHRINKER with the Shrinker package e.g.
*      $shrinker and keep all other default values.
*      That will generate 2 repository objects with name starting with
*      ZSHRINKER_DEMO_SHRINKER_*, which contain all Shrinker code.
*   5. Create manually empty repository objects and activate them:
*      - Executable program ZSHRINKER_ABAPGIT_DEF
*      - Include ZSHRINKER_ABAPGIT_IMP1
*      - Include ZSHRINKER_ABAPGIT_IMP2
*      - Include ZSHRINKER_ABAPGIT_IMP3
*      - Include ZSHRINKER_ABAPGIT_IMP4
*      - Include ZSHRINKER_ABAPGIT_IMP5
*   6. Run the program ZSHRINKER_DEMO_ABAPGIT with the following parameters
*      - abapGit installation packages in your system:
*            $abapgit*
*      - Include for class and interface definitions:
*            ZSHRINKER_ABAPGIT_DEF
*      - Prefix of includes 1 to 5 for class implementations:
*            ZSHRINKER_ABAPGIT_IMP
*      - standalone program:
*            empty
*      - Include program containing the license and notice:
*
*      That will generate the 6 repository objects with names starting with
*      ZSHRINKER_ABAPGIT*, which contain all abapGit code.
*
********************************************************************************
