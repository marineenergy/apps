# apps

Main Shiny app: [shiny.marineenergy.app/report](https://shiny.marineenergy.app/report)

This README with html listing: [https://marineenergy.github.io/apps](https://marineenergy.github.io/apps)

## html

These web pages (\*.html) are typically rendered from Rmarkdown (\*.Rmd):

<!-- Jekyll rendering: https://marineenergy.github.io/apps/ -->
{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}

## development

Normally, on command line, get this repo:

```bash
git clone https://github.com/mareineenergy/apps
```

Or in Rstudio, File -> New Project... -> Version Control -> git ...

Might need to install [Git](https://git-scm.com/).

## restart shiny service

```bash
# /etc/services.d
# /etc/init.d/
sudo service shiny-server restart
```

### check if bad gateway and automatically restart 

```bash
# install monit
sudo apt-get install monit
sudo service monit start
sudo service monit status

sudo systemctl enable --now monit
sudo /etc/init.d/monit start
sudo systemctl status monit
# Monit: the monit HTTP interface is not enabled, please add the 'set httpd' statement and use the 'allow' option to allow monit to connect

sudo -u shiny pm2 start /usr/bin/shiny-server
sudo -u shiny pm2 save
sudo -u shiny pm2 list
sudo -u shiny pm2 logs shiny-server
sudo -u shiny pm2 start shiny-server

ps -eaf | grep shiny
# root       265     1  0 Apr15 ?        00:00:00 s6-supervise shiny-server
# root       269   265  0 Apr15 ?        00:00:22 /opt/shiny-server/ext/node/bin/shiny-server /opt/shiny-server/lib/main.js
# root       282   269  0 Apr15 ?        00:00:43 xtail /var/log/shiny-server/
# shiny      592     1  0 Apr15 ?        00:00:13 PM2 v5.1.2: God Daemon (/home/shiny/.pm2)
# shiny      970   592  0 Apr15 ?        00:02:00 /usr/local/lib/R/bin/exec/R --no-save --no-restore --no-echo --no-restore --file=/share/github/api/run-api.R
# root      2052   269  0 Apr19 ?        00:00:00 su -s /bin/bash --login -p -- shiny -c cd \/srv\/shiny-server\/report-v2 && R --no-save --slave -f \/opt\/shiny-server\/R\/SockJSAdapter\.R
# shiny     2053  2052 66 Apr19 ?        08:22:04 /usr/local/lib/R/bin/exec/R --no-save --no-restore --no-save --slave -f /opt/shiny-server/R/SockJSAdapter.R
# bbest     5757  2943  0 00:52 pts/0    00:00:00 grep --color=auto shiny
sudo kill -9 265
sudo kill -9 269
sudo kill 11814
sudo killall 12279
sudo killall xtail
sudo killall /opt/shiny-server/ext/node/bin/shiny-server
sudo killall s6-supervise shiny-server
sudo kill -9 13125

sudo kill -9 2053   # /usr/local/lib/R/bin/exec/R --no-save --no-restore --no-save --slave -f /opt/shiny-server/R/SockJSAdapter.R
sudo kill -9 13126  # /opt/shiny-server/ext/node/bin/shiny-server /opt/shiny-server/lib/main.js
sudo kill -9 13207
sudo ps -eaf | grep shiny


ps aux | grep -E "^shiny\s*[0-9]*.*" | sed -r 's/shiny\s*([0-9]*).*/\1/' | while read i ; do sudo kill -9 "$i" ; done ;

sudo ps -eaf | grep shiny
sudo pkill -P 11814
netstat -tulpn


pm2 start exec /usr/sbin/shiny-server 2>&1
``
## todo

To get Rmd to show, eg https://mhk-env.github.io/shiny-apps/ply2rwhale.html, had to delete sym linked index.html that pointed to `/opt/shiny-server/samples/welcome.html`.
