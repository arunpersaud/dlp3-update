from datetime import datetime


def create_new_spec_file(directory, name, version, ending):
    filename = directory / f"python-{name}" / f"python-{name}.spec"
    with filename.open("w") as f:
        f.write("#\n")
        f.write(f"# spec file for package python-{name}\n")
        f.write("#\n")
        f.write(
            f"# Copyright (c) {datetime.now().year} SUSE LINUX GmbH, Nuernberg, Germany.\n"
        )
        f.write("#\n")
        f.write(
            "# All modifications and additions to the file contributed by third parties\n"
        )
        f.write(
            "# remain the property of their copyright owners, unless otherwise agreed\n"
        )
        f.write(
            "# upon. The license for this file, and modifications and additions to the\n"
        )
        f.write(
            "# file, is the same license as for the pristine package itself (unless the\n"
        )
        f.write(
            "# license for the pristine package is not an Open Source License, in which\n"
        )
        f.write(
            '# case the license is the MIT License). An "Open Source License" is a\n'
        )
        f.write("# license that conforms to the Open Source Definition (Version 1.9)\n")
        f.write("# published by the Open Source Initiative.\n")
        f.write("\n")
        f.write("# Please submit bugfixes or comments via http://bugs.opensuse.org/\n")
        f.write("#\n")
        f.write("\n")
        f.write("\n")
        f.write(
            "%{?!python_module:%define python_module() python-%{**} python3-%{**}}\n"
        )
        f.write(f"Name:           python-{name}\n")
        f.write(f"Version:        {version}\n")
        f.write("Release:        0\n")
        f.write("Summary:        \n")
        f.write("License:        \n")
        f.write("Group:          Development/Languages/Python\n")
        f.write("Url:            \n")
        f.write(
            "Source:         https://files.pythonhosted.org/packages/source/{}/{}/{}-%{{version}}{}\n".format(
                name[0], name, name, ending
            )
        )
        f.write("BuildRequires:  %{python_module devel}\n")
        f.write("BuildRequires:  %{python_module setuptools}\n")
        f.write("BuildRequires:  fdupes\n")
        if ending == ".zip":
            f.write("BuildRequires:  unzip\n")
        f.write("BuildRequires:  python-rpm-macros\n")
        f.write("BuildRoot:      %{_tmppath}/%{name}-%{version}-build\n")
        f.write("BuildArch:      noarch\n")
        f.write("%python_subpackages\n")
        f.write("\n")
        f.write("%description\n")
        f.write("\n")
        f.write("\n")
        f.write("%prep\n")
        f.write(f"%setup -q -n {name}-%{{version}}\n")
        f.write("\n")
        f.write("%build\n")
        f.write("%python_build\n")
        f.write("\n")
        f.write("%install\n")
        f.write("%python_install\n")
        f.write("%python_expand %fdupes %{buildroot}%{$python_sitelib}\n")
        f.write("\n")
        f.write("%check\n")
        f.write("%python_exec -m pytest test\n")
        f.write("\n")
        f.write("%files %{python_files}\n")
        f.write("%defattr(-,root,root,-)\n")
        f.write("%doc README.rst\n")
        f.write(f"%{{python_sitelib}}/{name}*\n")
        f.write(f"#%python3_only %{{_bindir}}/{name}\n")
        f.write(f"%{{_bindir}}/{name}\n")
        f.write("\n")
        f.write("%changelog\n")
