#!/usr/bin/python

from ConfigParser import SafeConfigParser
import socket
import sys
import os

WHEREAMI = os.path.abspath(os.path.dirname(__file__))
whoami = socket.gethostname()
name = whoami.split('.')[0]
config_dir = os.path.join(WHEREAMI, 'config')
ebin_dir = os.path.join(WHEREAMI, 'ebin')

CONFIG_TEMPLATE = """%%%%%%Generated configuration
[{kernel,
        [{distributed, [%(app_nodes)s]},
         {sync_nodes_mandatory, [%(mandatory_nodes)s]},
         {sync_nodes_timeout, %(sync_timeout)s}
        ]
}].
"""

KERNEL_TEMPLATE = """{%(name)s, %(start_timeout)s, [%(optional_nodes)s]}"""

APPLICATION_TEMPLATE = """%%%%%%Generated application
{application, %(name)s,
        [{vsn, "0.0.1"},
                {description, "%(description)s"},
                {modules, [carbon, carbon_sup, carbon_server]},
                {applications, [stdlib, kernel]},
                {registered, [carbon, carbon_sup, carbon_server]},
                {mod, {carbon, %(name)s}},
                {env, [%(env)s]}
        ]
}.
"""

def make_nodes(node_string, skip_me=False):
    nodes = list()
    for var in node_string.split(','):
        var = var.rstrip(' ').lstrip(' ')
        if skip_me and (var == whoami or var == name):
            continue
        sname = var.split('.')[0]
        fname = "%s@%s" % (sname, var)
        if fname in nodes: continue
        nodes.append(fname)
    return ', '.join(nodes)

class AppBuilder(object):
    application = property(
        lambda s: os.path.join(ebin_dir, "%s.app" % (s.name,)))
    def __init__(self, name, config):
        self.env = ''
        self.optional_nodes = ''
        self.__dict__.update(config)
        self.name = name
        self.optional_nodes = make_nodes(self.optional_nodes)

    @property
    def kernel_config(self):
        return KERNEL_TEMPLATE % vars(self)

    def write(self):
        fd = open(self.application, 'wb')
        fd.write(str(self))
        fd.close()

    def __repr__(self):
        return APPLICATION_TEMPLATE % vars(self)

    __str__ = __repr__

class Main(object):
    config = property(
        lambda s: os.path.join(config_dir, "%s.config" % (name,)))
    def __init__(self, config):
        self.__dict__.update(config)
        self._apps = set()
        self.mandatory_nodes = make_nodes(self.mandatory_nodes, skip_me=True)

    def add(self, app):
        self._apps.add(app)

    def write(self):
        self.app_nodes = ', '.join(i.kernel_config for i in self._apps)
        for app in self._apps: app.write()
        fd = open(self.config, 'wb')
        fd.write(str(self))
        fd.close()

    def __repr__(self):
        return CONFIG_TEMPLATE % vars(self)

    __str__ = __repr__

if __name__ == '__main__':
    cf = SafeConfigParser()
    cf.read(sys.argv[1])
    main_config = Main(cf.items('main'))
    names = list()
    for section in cf.sections():
        if section == 'main': continue
        names.append(section)
        main_config.add(AppBuilder(section, dict(cf.items(section))))
    main_config.write()
    os.execvpe('erl', [
        'erl', '-name', name, '-setcookie', 'nomnomnom', 
        '-config', os.path.join(config_dir, name),
        '-pa', ebin_dir, '-eval', 
        "inets:start(), %s" % ', '.join("application:start(%s)" % i for i in names)
        ], os.environ.copy()
    )
