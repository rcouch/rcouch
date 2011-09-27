# _revision, _release, and _version should be defined on the rpmbuild command
# line like so:
#
# --define "_version 0.9.1" --define "_release 7" \
# --define "_revision 0.9.1-19-abcdef"

Name: refuge
Version: %{_version}
Release: %{_release}%{?dist}
License: Apache License
Group: Development/Libraries
Source: http://github.org/downloads/refuge/refuge/%{name}-%{_revision}.tar.gz
Source1: refuge_init
URL: http://refuge.io
Vendor: The Refuge project 
Packager: Refuge <refuge@lists.refuge.io>>
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Summary: Refuge, The decentralized data platform

%description
Riak is a distributed data store.

%define refuge_lib %{_libdir}/%{name}
%define init_script %{_sysconfdir}/init.d/%{name}

%define __prelink_undo_cmd /bin/cat prelink library
%define debug_package %{nil}

%define platform_bin_dir %{_sbindir}
%define platform_data_dir %{_localstatedir}/lib/%{name}
%define platform_etc_dir %{_sysconfdir}/%{name}
%define platform_lib_dir %{refuge_lib}
%define platform_log_dir %{_localstatedir}/log/%{name}

%prep
%setup -q -n %{name}-%{_revision}
cat > rel/vars.config <<EOF
% Platform-specific installation paths
{platform_bin_dir,  "/usr/sbin"}.
{platform_data_dir, "%{platform_data_dir}"}.
{platform_view_dir, "%{platform_data_dir}"}.
{platform_etc_dir,  "%{platform_etc_dir}"}.
{platform_lib_dir,  "%{platform_lib_dir}"}.
{platform_log_dir,  "%{platform_log_dir}""}.
{platform_run_dir,  "%{_localstatedir}/run/%{name}/"}.
{platform_share_dir, "%{_datadir}/%{name}/"}.

%% app config
{couchdb_ip, "0.0.0.0"}.
{couchdb_port, 15984}.
{ssl_port, 16986}.
{sasl_error_log,        ""%{platform_log_dir}/sasl-error.log"}.
{sasl_log_dir,          ""%{platform_log_dir}/sasl"}.

% vm.args
{node,         "refuge@127.0.0.1"}.
{crash_dump,   ""%{platform_log_dir}/erl_crash.dump"}.

% bin/refuge*
{runner_script_dir,  "%{platform_bin_dir}"}.
{runner_base_dir,    "%{platform_lib_dir}"}.
{runner_etc_dir,     "%{platform_etc_dir}"}.
{runner_log_dir,     "%{platform_log_dir}"}.
{pipe_dir,           "%{_localstatedir}/run/%{name}/"}.
{runner_user,        "%{name}"}.
EOF

%build
mkdir %{name}
make rel

%install
mkdir -p %{buildroot}%{platform_etc_dir}
mkdir -p %{buildroot}%{platform_lib_dir}
mkdir -p %{buildroot}%{_datadir}/%{name}
mkdir -p %{buildroot}%{platform_log_dir}
mkdir -p %{buildroot}%{platform_log_dir}/sasl
mkdir -p %{buildroot}%{_localstatedir}/run/%{name}
mkdir -p %{buildroot}%{platform_data_dir}/mr_queue

#Copy all necessary lib files etc.
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/lib %{buildroot}%{platform_lib_dir}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/erts-* \
		%{buildroot}%{platform_lib_dir}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/releases \
		%{buildroot}%{platform_lib_dir}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/share \
		%{buildroot}%{_datadir}/%{name}
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/app.config \
	%{buildroot}%{platform_etc_dir}/
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/vm.args \
	%{buildroot}%{platform_etc_dir}/
install -p -D -m 0755 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/%{name} \
	%{buildroot}/%{platform_bin_dir}/%{name}
install -p -D -m 0755 %{SOURCE1} %{buildroot}/%{init_script}

# Needed to work around check-rpaths which seems to be hardcoded into recent
# RPM releases
export QA_RPATHS=3


%pre
# create refuge group only if it doesn't already exist
if ! getent group refuge >/dev/null 2>&1; then
        groupadd -r refuge
fi

# create refuge user only if it doesn't already exist
if ! getent passwd refuge >/dev/null 2>&1; then
        useradd -r -g refuge --home %{platform_data_dir} refuge
        usermod -c "Refuge" 
fi

%post
# Fixup perms for SELinux
find %{platform_lib_dir} -name "*.so" -exec chcon -t textrel_shlib_t {} \;

%files
%defattr(-,refuge,refuge)
%attr(-,root,root) %{_libdir}/*
%dir %{platform_etc_dir}
%config(noreplace) %{platform_etc_dir}/*
%attr(0755,root,root) %{init_script}
%attr(0755,root,root) %{platform_bin_dir}/%{name}
%attr(0755,root,root) %{platform_bin_dir}/%{name}-admin
%attr(0755,root,root) %{platform_bin_dir}/search-cmd
%attr(0644,root,root) %{_datadir}/%{name}/www/*
%attr(0644,root,root) %{_datadir}/%{name}/server/*
%{platform_data_dir}
%{platform_log_dir}
%{_localstatedir}/run/%{name}

%clean
rm -rf %{buildroot}
