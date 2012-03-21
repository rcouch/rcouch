# _revision, _release, and _version should be defined on the rpmbuild command
# line like so:
#
# --define "_version 0.9.1" --define "_release 7" \
# --define "_revision 0.9.1-19-abcdef"

Name: rcouch
Version: %{_version}
Release: %{_release}%{?dist}
License: Apache License
Group: Development/Libraries
Source: http://github.org/downloads/refuge/rcouch/%{name}-%{_revision}.tar.gz
Source1: rcouch_init
URL: http://refuge.io
Vendor: The Refuge project
Packager: Refuge <refuge@lists.refuge.io>>
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Summary: Refuge Couchdb Distribution

BuildRequires: git
BuildRequires: make
BuildRequires: erlang
BuildRequires: gcc
BuildRequires: gcc-c++
BuildRequires: libstdc++-devel

%description
CouchDB is document oriented Database..

%define rcouch_lib %{_libdir}/%{name}
%define init_script %{_sysconfdir}/init.d/%{name}

%define __prelink_undo_cmd /bin/cat prelink library
%define debug_package %{nil}

%define platform_bin_dir %{_bindir}
%define platform_data_dir %{_localstatedir}/lib/%{name}
%define platform_etc_dir %{_sysconfdir}/%{name}
%define platform_lib_dir %{rcouch_lib}
%define platform_log_dir %{_localstatedir}/log/%{name}

%prep
%setup -q -n %{name}-%{_revision}
cat > rel/rcouch.config <<EOF
% Platform-specific installation paths
{platform_bin_dir,  "/usr/bin"}.
{platform_data_dir, "%{platform_data_dir}"}.
{platform_view_dir, "%{platform_data_dir}"}.
{platform_etc_dir,  "%{platform_etc_dir}"}.
{platform_lib_dir,  "%{platform_lib_dir}"}.
{platform_log_dir,  "%{platform_log_dir}"}.
{platform_run_dir,  "%{_localstatedir}/run/%{name}/"}.
{platform_share_dir, "%{_datadir}/%{name}/"}.

%% vendor info
{vendor_name, "rcouch"}.

%% app config
{couchdb_ip, "0.0.0.0"}.
{couchdb_port, 5984}.
{ssl_port, 6986}.
{sasl_error_log, "%{platform_log_dir}/sasl-error.log"}.
{sasl_log_dir, "%{platform_log_dir}/sasl"}.

% vm.args
{node, "rcouch@127.0.0.1"}.
{crash_dump, "%{platform_log_dir}/erl_crash.dump"}.

% bin/rcouch*
{runner_script_dir, "%{platform_bin_dir}"}.
{runner_base_dir, "%{platform_lib_dir}"}.
{runner_etc_dir, "%{platform_etc_dir}"}.
{runner_log_dir, "%{platform_log_dir}"}.
{pipe_dir, "%{_localstatedir}/run/%{name}/"}.
{runner_user, "%{name}"}.
EOF

%build
mkdir %{name}
make rel USE_STATIC_ICU=1

%install
mkdir -p %{buildroot}%{platform_data_dir}
mkdir -p %{buildroot}%{platform_etc_dir}
mkdir -p %{buildroot}%{platform_lib_dir}
mkdir -p %{buildroot}%{_datadir}/%{name}
mkdir -p %{buildroot}%{platform_log_dir}
mkdir -p %{buildroot}%{platform_log_dir}/sasl
mkdir -p %{buildroot}%{_localstatedir}/run/%{name}

#Copy all necessary lib files etc.
cp -R $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/lib \
        %{buildroot}%{platform_lib_dir}
cp -R $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/erts* \
		%{buildroot}%{platform_lib_dir}
cp -R $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/releases \
		%{buildroot}%{platform_lib_dir}
cp -R $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/share/* \
		%{buildroot}%{_datadir}/%{name}/
chmod 0755 %{buildroot}%{platform_lib_dir}/erts*/bin/*
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/default.ini \
	%{buildroot}%{platform_etc_dir}/
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/local.ini \
	%{buildroot}%{platform_etc_dir}/
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/app.config \
	%{buildroot}%{platform_etc_dir}/
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/vm.args \
	%{buildroot}%{platform_etc_dir}/
install -p -D -m 0755 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/%{name} \
	%{buildroot}/%{platform_bin_dir}/%{name}
install -p -D -m 0755 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/couchjs \
	%{buildroot}/%{platform_bin_dir}/couchjs
install -p -D -m 0755 %{SOURCE1} %{buildroot}/%{init_script}

# Needed to work around check-rpaths which seems to be hardcoded into recent
# RPM releases
export QA_RPATHS=3


%pre
# create rcouch group only if it doesn't already exist
if ! getent group rcouch >/dev/null 2>&1; then
        groupadd -r rcouch
fi

# create rcouch user only if it doesn't already exist
if ! getent passwd rcouch >/dev/null 2>&1; then
        useradd -r -g rcouch --home %{platform_data_dir} rcouch
        usermod -c "Refuge CouchDB" rcouch
fi

%post
# Fixup perms for SELinux. If SELinux disabled warnings will be emitted
find %{platform_lib_dir} -name "*.so" -exec chcon -t textrel_shlib_t {} \;

%files
%defattr(-,rcouch,rcouch)
%attr(-,root,root) %{_libdir}/*
%dir %{platform_etc_dir}
%config(noreplace) %{platform_etc_dir}/*
%attr(0755,root,root) %{init_script}
%attr(0755,root,root) %{platform_lib_dir}/erts*/bin/*
%attr(0755,root,root) %{platform_bin_dir}/%{name}
%attr(0755,root,root) %{platform_bin_dir}/couchjs
%attr(0755,root,root) %{_datadir}/%{name}/www/*
%attr(0755,root,root) %{_datadir}/%{name}/server/*
%{platform_data_dir}
%{platform_log_dir}
%{_localstatedir}/run/%{name}

%clean
rm -rf %{buildroot}
