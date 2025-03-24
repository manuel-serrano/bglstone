;*=====================================================================*/
;*    .../project/bglstone/src/bigloo/bigloo/Engine/configure.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 29 09:31:00 2000                          */
;*    Last change :  Thu Mar  6 14:16:05 2025 (serrano)                */
;*    Copyright   :  2000-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The machine dependent configuration.                             */
;*    -------------------------------------------------------------    */
;*    In order to avoid daunting bootstrap problem, I have decided not */
;*    to produce this file automatically. It is written and maintained */
;*    by (my) hand.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_configure
   
   (extern (macro bgl-configure-shell::string "SHELL")
           (macro bgl-configure-c-compiler-style::string "C_COMPILER_STYLE")
	   (macro bgl-configure-c-compiler::string "C_COMPILER")
	   (macro bgl-configure-c-compiler-o-option::string "C_COMPILER_O_OPTION")
           (macro bgl-configure-c-compiler-debug-option::string "C_COMPILER_DEBUG_OPTION")
	   (macro bgl-configure-c-compiler-optim-flag::string "C_COMPILER_OPTIM_FLAGS")
	   (macro bgl-configure-c-flag::string "C_FLAGS")
	   (macro bgl-configure-c-strip-flag::string "C_STRIP_FLAGS")
	   (macro bgl-configure-c-prof-flag::string "C_PROFILE_FLAGS")
           (macro bgl-configure-c-object-file-extension::string "C_OBJECT_FILE_EXTENSION")
	   (macro bgl-configure-c-string-split::bool "C_STRING_SPLIT")
           (macro bgl-configure-c-linker-style::string "C_LINKER_STYLE")
	   (macro bgl-configure-c-linker-o-option::string "C_LINKER_O_OPTION")
           (macro bgl-configure-c-linker-debug-option::string "C_LINKER_DEBUG_OPTION")
	   (macro bgl-configure-c-linker-optim-flags::string "C_LINKER_OPTIM_FLAGS")
	   (macro bgl-configure-ld-library-dir::string "BGL_LD_LIBRARY_DIR")
	   (macro bgl-configure-library-directory::string "LIBRARY_DIRECTORY")
 	   (macro bgl-configure-zip-directory::string "ZIP_DIRECTORY")
 	   (macro bgl-configure-dll-directory::string "DLL_DIRECTORY")
	   (macro bgl-configure-user-libraries::string "USER_LIBRARIES")
	   (macro bgl-configure-c-beautifier::string "C_BEAUTIFIER")
	   (macro bgl-configure-dirname-cmd::string "DIRNAME_CMD")
	   (macro bgl-configure-library-base-name::string "LIBRARY_BASE_NAME")
	   (macro bgl-configure-release-number::string "BGL_RELEASE_NUMBER")
	   (macro bgl-configure-specific-version::string "BGL_SPECIFIC_VERSION")
	   (macro bgl-configure-heap-debug-copt::string "BGL_HEAP_DEBUG_COPT")
	   (macro bgl-configure-shared-library-available?::bool "HAVE_SHARED_LIBRARY")
	   (macro bgl-configure-shared-link-option::string "ADDITIONAL_SHARED_LINK_OPTION")
	   (macro bgl-configure-static-link-option::string "ADDITIONAL_STATIC_LINK_OPTION")
	   (macro bgl-configure-dlopen?::bool "HAVE_DLOPEN")
	   (macro bgl-configure-dlopen-lib::string "DLOPEN_LD_OPT")
	   (macro bgl-configure-have-bigloo-abort::bool "BGL_HAVE_BIGLOO_ABORT")
	   (macro bgl-configure-java::string "BGL_JAVA")
	   (macro bgl-configure-jar::string "BGL_JAR")
	   (macro bgl-configure-java-shell::string "BGL_JAVA_SHELL")
	   (macro bgl-configure-jflags::string "BGL_JAVA_OPT")
	   (macro bgl-configure-jvflags::string "BGL_JAVA_VOPT")
	   (macro bgl-configure-dotnet-shell::string "\"none\"")
	   (macro bgl-configure-dotnet-ld::string "\"none\"")
	   (macro bgl-configure-dotnet-ld-style::string "\"none\"")
	   (macro bgl-configure-dotnet-clr::string "\"none\"")
	   (macro bgl-configure-dotnet-clr-style::string "\"none\"")
	   (macro bgl-configure-dotnet-clr-opt::string "\"none\"")
	   (macro bgl-configure-dotnet-asm::string "\"none\"")
	   (macro bgl-configure-default-back-end::string "BGL_DEFAULT_BACK_END")
	   (macro bgl-configure-gc-lib::string "BGL_GC_LIBRARY")
	   (macro bgl-configure-gc-custom::bool "BGL_GC_CUSTOM")
	   
	   (macro bgl-foreign-dlopen-init::string "BGL_DYNAMIC_LOAD_INIT")
	   (macro bgl-configure-bdb-available?::bool "BGL_HAVE_BDB")
	   (macro bgl-foreign-BDB_LIBRARY_MAGIC_NUMBER::int "BDB_LIBRARY_MAGIC_NUMBER"))
   
   (java   (class bgl-configure
	      (field static shell::string "SHELL")
	      (field static c-compiler-style::string "C_COMPILER_STYLE")
              (field static c-compiler::string "C_COMPILER")
	      (field static c-compiler-o-option::string "C_COMPILER_O_OPTION")
              (field static c-compiler-debug-option::string "C_COMPILER_DEBUG_OPTION")
	      (field static c-compiler-optim-flag::string "C_COMPILER_OPTIM_FLAGS")
	      (field static c-flag::string "C_FLAGS")
	      (field static c-strip-flag::string "C_STRIP_FLAGS")
	      (field static c-prof-flag::string "C_PROFILE_FLAGS")
              (field static c-object-file-extension::string "C_OBJECT_FILE_EXTENSION")
	      (field static c-string-split::bool "C_STRING_SPLIT")
              (field static c-linker-style::string "C_LINKER_STYLE")
	      (field static c-linker-o-option::string "C_LINKER_O_OPTION")
              (field static c-linker-debug-option::string "C_LINKER_DEBUG_OPTION")
              (field static c-linker-optim-flags::string "C_LINKER_OPTIM_FLAGS")
	      (field static ld-library-dir::string "LD_LIBRARY_DIR")
	      (field static library-directory::string "LIBRARY_DIRECTORY")
	      (field static zip-directory::string "ZIP_DIRECTORY")
	      (field static dll-directory::string "DLL_DIRECTORY")
	      (field static user-libraries::string "USER_LIBRARIES")
	      (field static c-beautifier::string "C_BEAUTIFIER")
	      (field static dirname-cmd::string "DIRNAME_CMD")
	      (field static library-base-name::string "LIBRARY_BASE_NAME")
	      (field static release-number::string "BGL_RELEASE_NUMBER")
	      (field static specific-version::string "BGL_SPECIFIC_VERSION")
	      (field static heap-debug-copt::string "BGL_HEAP_DEBUG_COPT")
	      (field static shared-library-available?::bool "HAVE_SHARED_LIBRARY")
	      (field static shared-link-option::string "ADDITIONAL_SHARED_LINK_OPTION")
	      (field static static-link-option::string "ADDITIONAL_STATIC_LINK_OPTION")
	      (field static dlopen?::bool "HAVE_DLOPEN")
	      (field static dlopen-lib::string "DLOPEN_LD_OPT")
	      (field static have-bigloo-abort::bool "BGL_HAVE_BIGLOO_ABORT")
	      (field static java::string "BGL_JAVA")
	      (field static jar::string "BGL_JAR")
	      (field static java-shell::string "BGL_JAVA_SHELL")
	      (field static jflags::string "BGL_JAVA_OPT")
	      (field static jvflags::string "BGL_JAVA_VOPT")
	      (field static dotnet-shell::string "BGL_DOTNET_SHELL")
	      (field static dotnet-ld::string "BGL_DOTNET_LD")
	      (field static dotnet-ld-style::string "BGL_DOTNET_LD_STYLE")
	      (field static dotnet-clr::string "BGL_DOTNET_CLR")
	      (field static dotnet-clr-style::string "BGL_DOTNET_CLR_STYLE")
	      (field static dotnet-clr-opt::string "BGL_DOTNET_CLR_OPT")
	      (field static dotnet-asm::string "BGL_DOTNET_ASM")
	      (field static default-back-end::string "BGL_DEFAULT_BACK_END")
	      (field static gc-lib::string "BGL_GC_LIBRARY")
	      (field static gc-custom::bool "BGL_GC_CUSTOM")
	      (field static bdb-available?::bool "BGL_HAVE_BDB")
	      "bigloo.configure")
	   
	   (class bgl-foreign
	      (field static dlopen-init::string "BGL_DYNAMIC_LOAD_INIT")
	      (field static BDB_LIBRARY_MAGIC_NUMBER::int "BDB_LIBRARY_MAGIC_NUMBER")
	      "bigloo.foreign")))
 
