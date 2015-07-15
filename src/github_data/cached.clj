(ns github-data.cached)

(def commit-count-by-repo
  '(76405 5901 22798 83930 57124 74539 87741 93680 112441 48726 101431 81765))

(def over-fifteen-forks (list '([13 "+repo:Redth/PushSharp"] [0 "+repo:JamesNK/Newtonsoft.Json"] [205 "+repo:aspnet/Mvc"] [7 "+repo:opserver/Opserver"] [124 "+repo:ShareX/ShareX"] [17 "+repo:shadowsocks/shadowsocks-csharp"] [11 "+repo:MehdiK/Humanizer"] [105 "+repo:scriptcs/scriptcs"] [43 "+repo:EventStore/EventStore"] [139 "+repo:NLog/NLog"] [60 "+repo:HangfireIO/Hangfire"] [0 "+repo:EsotericSoftware/spine-runtimes"] [0 "+repo:akkadotnet/akka.net"] [19 "+repo:madskristensen/WebEssentials2013"] [4 "+repo:umbraco/Umbraco-CMS"] [0 "+repo:Moq/moq4"] [0 "+repo:michael-wolfenden/Polly"] [22 "+repo:playgameservices/play-games-plugin-for-unity"] [406 "+repo:aspnet/dnx"] [46 "+repo:Squirrel/OldSquirrelForWindows"] [1 "+repo:kevin-montrose/Jil"] [17 "+repo:octokit/octokit.net"] [1 "+repo:Fody/Fody"] [0 "+repo:xunit/xunit"] [24 "+repo:aspnetboilerplate/aspnetboilerplate"] [34 "+repo:thedillonb/CodeHub"] [37 "+repo:AutoFixture/AutoFixture"] [35 "+repo:MattRix/Futile"] [0 "+repo:paulcbetts/refit"] [2 "+repo:madskristensen/MiniBlog"] [5 "+repo:brockallen/BrockAllen.MembershipReboot"] [0 "+repo:xebecnan/UniLua"] [16 "+repo:Jessecar96/SteamBot"] [9 "+repo:Grabacr07/KanColleViewer"] [21 "+repo:viperneo/winforms-modernui"] [17 "+repo:jstedfast/MailKit"] [0 "+repo:MarlabsInc/SocialGoal"] [26 "+repo:serilog/serilog"] [0 "+repo:johnculviner/jquery.fileDownload"] [3 "+repo:gsscoder/commandline"] [18 "+repo:JimBobSquarePants/ImageProcessor"] [0 "+repo:loresoft/msbuildtasks"] [12 "+repo:lzybkr/PSReadLine"] [4 "+repo:SteamRE/SteamKit"] [13 "+repo:TouchScript/TouchScript"] [0 "+repo:mspnp/cqrs-journey"] [145 "+repo:MediaBrowser/Emby"] [0 "+repo:opendns/dnscrypt-win-client"] [13 "+repo:lukebuehler/CShell"] [0 "+repo:JeffreySu/WeiXinMPSDK"] [0 "+repo:domaindrivendev/Swashbuckle"] [0 "+repo:zeromq/netmq"] [0 "+repo:accord-net/framework"] [0 "+repo:Reactive-Extensions/Rx.NET"] [0 "+repo:WebApiContrib/WebAPIContrib"] [81 "+repo:sharpdx/SharpDX"] [0 "+repo:ScutGame/Scut"] [0 "+repo:sixthsense/sixthsense"] [2 "+repo:umeng/umeng-muti-channel-build-tool"] [15 "+repo:pbhogan/InControl"] [8 "+repo:MiniProfiler/dotnet"] [49 "+repo:fdorg/flashdevelop"] [16 "+repo:IdentityServer/IdentityServer2"] [0 "+repo:koush/UniversalAdbDriver"] [60 "+repo:AdamsLair/duality"] [1 "+repo:NLua/NLua"] [10 "+repo:Code52/pretzel"] [13 "+repo:dennisdoomen/fluentassertions"] [0 "+repo:Bobris/Nowin"] [0 "+repo:ASP-NET-MVC/aspnetwebstack"] [1 "+repo:IdentityModel/Thinktecture.IdentityModel.45"] [2 "+repo:filipw/AspNetWebApi-OutputCache"] [25 "+repo:dockpanelsuite/dockpanelsuite"] [0 "+repo:Haacked/SeeGit"] [0 "+repo:VerbalExpressions/CSharpVerbalExpressions"] [6 "+repo:TestStack/White"] [19 "+repo:Pash-Project/Pash"] [12 "+repo:timheuer/callisto"] [3 "+repo:soomla/unity3d-store"] [0 "+repo:sprache/Sprache"] [37 "+repo:jstedfast/MimeKit"] [104 "+repo:adamdriscoll/poshtools"] [0 "+repo:NetEase/UnitySocketIO"] [0 "+repo:paulcbetts/splat"] [0 "+repo:kerryjiang/SuperSocket"] [13 "+repo:charlesw/tesseract"] [0 "+repo:prime31/GoKit"] [19 "+repo:aws/aws-sdk-net"] [12 "+repo:mono/CppSharp"] [0 "+repo:nunit/nunit"] [20 "+repo:aliostad/CacheCow"] [5 "+repo:xupefei/Locale-Emulator"] [0 "+repo:erichexter/twitter.bootstrap.mvc"] [0 "+repo:et1337/Lemma"] [3 "+repo:Code52/carnac"] [2 "+repo:MuMech/MechJeb2"] [0 "+repo:prime31/CharacterController2D"] [0 "+repo:jgeurts/FluentScheduler"] [0 "+repo:gree/unity-webview"] [20 "+repo:FlorianRappl/AngleSharp"]) '([0 "+repo:ZachBray/FunScript"] [0 "+repo:AshleyF/VimSpeak"] [6 "+repo:fsprojects/ProjectScaffold"] [0 "+repo:BlueMountainCapital/Deedle"] [3 "+repo:fscheck/FsCheck"] [7 "+repo:lefthandedgoat/canopy"] [0 "+repo:tpetricek/FSharp.Formatting"] [221 "+repo:intellifactory/websharper"] [0 "+repo:nessos/FsPickler"] [3 "+repo:freya-fs/freya"] [0 "+repo:BlueMountainCapital/FSharpRProvider"] [0 "+repo:sergey-tihon/Stanford.NLP.NET"] [6 "+repo:fsprojects/Cricket"] [2 "+repo:fsprojects/SQLProvider"] [36 "+repo:fsharp/FSharp.Compiler.Service"] [2 "+repo:fslaborg/FSharp.Charting"] [3 "+repo:fsprojects/FSharp.Data.SqlClient"] [0 "+repo:nessos/UnionArgParser"] [0 "+repo:dungpa/fantomas"] [6 "+repo:fsprojects/FSharpCommunityTemplates"] [0 "+repo:mathias-brandewinder/Machine-Learning-In-Action"] [2 "+repo:fsprojects/FSharp.TypeProviders.StarterPack"] [0 "+repo:fslaborg/FsLab"] [0 "+repo:fsprojects/FSharpx.Collections"] [0 "+repo:markusl/GoogleTestRunner"] [0 "+repo:sergey-tihon/Stanford.NLP.Fsharp"] [0 "+repo:achrissmith/Programming-FS-Examples"]) '([17 "+repo:LightTable/LightTable"] [11 "+repo:omcljs/om"] [0 "+repo:aphyr/riemann"] [0 "+repo:swannodette/mori"] [0 "+repo:quil/quil"] [15 "+repo:pedestal/pedestal"] [0 "+repo:reagent-project/reagent"] [2 "+repo:magomimmo/modern-cljs"] [0 "+repo:clojure-cookbook/clojure-cookbook"] [1 "+repo:Engelberg/instaparse"] [0 "+repo:frankiesardo/icepick"] [0 "+repo:Prismatic/plumbing"] [3 "+repo:Prismatic/schema"] [0 "+repo:aphyr/jepsen"] [0 "+repo:clojure/core.async"] [2 "+repo:cemerick/friend"] [6 "+repo:jonase/kibit"] [0 "+repo:noprompt/frak"] [20 "+repo:Factual/drake"] [25 "+repo:clojure-liberator/liberator"] [16 "+repo:onyx-platform/onyx"] [0 "+repo:stuartsierra/component"] [0 "+repo:schani/clojurec"] [1 "+repo:krisajenkins/yesql"] [0 "+repo:clojure/core.typed"] [0 "+repo:Prismatic/dommy"] [1 "+repo:oakes/play-clj"] [1 "+repo:ptaoussanis/timbre"] [4 "+repo:oakes/Nightcode"] [0 "+repo:pkamenarsky/atea"] [8 "+repo:puniverse/pulsar"] [26 "+repo:clojure-android/lein-droid"] [1 "+repo:ptaoussanis/carmine"] [1 "+repo:noprompt/garden"] [3 "+repo:cemerick/austin"] [7 "+repo:tailrecursion/hoplon"] [0 "+repo:google/hesokuri"] [0 "+repo:puppetlabs/trapperkeeper"] [0 "+repo:noir-clojure/lib-noir"] [1 "+repo:oakes/Nightweb"] [11 "+repo:jonase/eastwood"] [2 "+repo:gf3/secretary"] [0 "+repo:clojure/test.check"] [3 "+repo:lynaghk/cljx"] [2 "+repo:tailrecursion/javelin"] [0 "+repo:weavejester/environ"] [0 "+repo:r0man/sablono"] [0 "+repo:ibdknox/jayq"] [0 "+repo:mcohen01/amazonica"] [0 "+repo:Datomic/codeq"] [0 "+repo:rbrush/clara-rules"] [25 "+repo:Netflix/PigPen"] [0 "+repo:juxt/bidi"] [2 "+repo:yogthos/Selmer"] [1 "+repo:pyr/cyanite"] [25 "+repo:mikera/core.matrix"] [0 "+repo:jkk/honeysql"] [0 "+repo:aysylu/loom"] [3 "+repo:liquidz/misaki"] [1 "+repo:reiddraper/simple-check"] [0 "+repo:Datomic/day-of-datomic"] [0 "+repo:yogthos/clj-pdf"] [3 "+repo:pedestal/app-tutorial"] [0 "+repo:clojurebook/ClojureProgramming"] [1 "+repo:elastic/es2unix"] [0 "+repo:Datomic/simulant"] [3 "+repo:james-henderson/chord"] [0 "+repo:cemerick/piggieback"] [0 "+repo:drcode/webfui"] [2 "+repo:JulianBirch/cljs-ajax"] [0 "+repo:juxt/jig"] [0 "+repo:gigasquid/clj-drone"] [1 "+repo:adamtornhill/code-maat"] [4 "+repo:ptaoussanis/tower"] [0 "+repo:swannodette/mies"] [0 "+repo:ibdknox/crate"] [0 "+repo:leonardoborges/bouncer"] [0 "+repo:xeqi/kerodon"] [0 "+repo:luminus-framework/luminus-template"] [4 "+repo:clojure-emacs/cider-nrepl"] [4 "+repo:cemerick/clojurescript.test"] [0 "+repo:r0man/cljs-http"] [0 "+repo:LightTable/fetch"] [2 "+repo:elastic/stream2es"] [11 "+repo:overtone/shadertone"] [0 "+repo:weavejester/clj-aws-s3"] [0 "+repo:stuartsierra/reloaded"] [0 "+repo:xavi/noir-auth-app"] [7 "+repo:cfpb/qu"] [0 "+repo:bodil/cljs-noderepl"] [0 "+repo:uncomplicate/fluokitten"] [1 "+repo:qiuxiafei/zk-web"] [5 "+repo:technomancy/syme"] [0 "+repo:krisc/events"] [0 "+repo:edgecase/dieter"] [2 "+repo:Raynes/laser"] [0 "+repo:ring-clojure/ring-json"] [0 "+repo:flyingmachine/brave-clojure-web"] [0 "+repo:dakrone/itsy"] [0 "+repo:marick/fp-oo"]) '([0 "+repo:meteor/meteor"] [0 "+repo:facebook/react"] [1 "+repo:airbnb/javascript"] [0 "+repo:Semantic-Org/Semantic-UI"] [0 "+repo:driftyco/ionic"] [1 "+repo:getify/You-Dont-Know-JS"] [739 "+repo:TryGhost/Ghost"] [66 "+repo:nnnick/Chart.js"] [17 "+repo:gulpjs/gulp"] [105 "+repo:select2/select2"] [136 "+repo:bower/bower"] [8 "+repo:hammerjs/hammer.js"] [24 "+repo:balderdashy/sails"] [25 "+repo:twitter/typeahead.js"] [8 "+repo:designmodo/Flat-UI"] [96 "+repo:lodash/lodash"] [39 "+repo:alvarotrigo/fullPage.js"] [0 "+repo:ftlabs/fastclick"] [0 "+repo:angular-ui/bootstrap"] [0 "+repo:rstacruz/nprogress"] [0 "+repo:photonstorm/phaser"] [10 "+repo:Shopify/dashing"] [61 "+repo:Unitech/PM2"] [0 "+repo:wagerfield/parallax"] [30 "+repo:enyo/dropzone"] [0 "+repo:typicode/json-server"] [285 "+repo:GoodBoyDigital/pixi.js"] [68 "+repo:adam-p/markdown-here"] [1 "+repo:peachananr/onepage-scroll"] [12 "+repo:NUKnightLab/TimelineJS"] [103 "+repo:angular-ui/ui-router"] [49 "+repo:ecomfe/echarts"] [0 "+repo:WickyNilliams/headroom.js"] [0 "+repo:scottjehl/picturefill"] [5 "+repo:petkaantonov/bluebird"] [3 "+repo:linnovate/mean"] [39 "+repo:sdelements/lets-chat"] [39 "+repo:riot/riot"] [0 "+repo:kamens/jQuery-menu-aim"] [29 "+repo:adobe-webplatform/Snap.svg"] [51 "+repo:postcss/autoprefixer"] [27 "+repo:eternicode/bootstrap-datepicker"] [9 "+repo:dimsemenov/Magnific-Popup"] [5 "+repo:koajs/koa"] [15 "+repo:brianreavis/selectize.js"] [24 "+repo:afaqurk/linux-dash"] [65 "+repo:webpack/webpack"] [10 "+repo:flightjs/flight"] [0 "+repo:substack/stream-handbook"] [64 "+repo:mgonto/restangular"] [21 "+repo:guillaumepotier/Parsley.js"] [202 "+repo:grafana/grafana"] [63 "+repo:benweet/stackedit"] [0 "+repo:ccampbell/mousetrap"] [0 "+repo:mailcheck/mailcheck"] [0 "+repo:kkga/spacegray"] [0 "+repo:FredrikNoren/ungit"] [0 "+repo:marmelab/gremlins.js"] [33 "+repo:breach/breach_core"] [14 "+repo:jessepollak/card"] [164 "+repo:hexojs/hexo"] [300 "+repo:google/traceur-compiler"] [4 "+repo:jakiestfu/Snap.js"] [27 "+repo:yyx990803/vue"] [41 "+repo:janpaepke/ScrollMagic"] [25 "+repo:amsul/pickadate.js"] [113 "+repo:mgcrea/angular-strap"] [0 "+repo:nolimits4web/Swiper"] [44 "+repo:yabwe/medium-editor"] [12 "+repo:ducksboard/gridster.js"] [0 "+repo:aosabook/500lines"] [37 "+repo:mozilla/togetherjs"] [4 "+repo:angular-app/angular-app"] [14 "+repo:feross/webtorrent"] [4 "+repo:NetEase/pomelo"] [24 "+repo:tripit/slate"] [24 "+repo:jacomyal/sigma.js"] [50 "+repo:BrowserSync/browser-sync"] [3 "+repo:square/crossfilter"] [0 "+repo:ExactTarget/fuelux"] [0 "+repo:Reactive-Extensions/RxJS"] [97 "+repo:zeroclipboard/zeroclipboard"] [0 "+repo:loadfive/Knwl.js"] [23 "+repo:mozilla/localForage"] [993 "+repo:NodeBB/NodeBB"] [241 "+repo:masayuki0812/c3"] [0 "+repo:alexwolfe/Buttons"] [2 "+repo:jschr/bootstrap-modal"] [24 "+repo:maroslaw/rainyday.js"] [0 "+repo:tommoor/tinycon"] [0 "+repo:JacksonTian/fks"] [19 "+repo:vitalets/x-editable"] [0 "+repo:taye/interact.js"] [8 "+repo:matthieua/WOW"] [29 "+repo:RubaXa/Sortable"] [117 "+repo:strongloop/loopback"] [2 "+repo:VerbalExpressions/JSVerbalExpressions"] [0 "+repo:componentjs/component"] [22 "+repo:imsky/holder"] [297 "+repo:ractivejs/ractive"]) '([1236 "+repo:atom/atom"] [0 "+repo:morrisjs/morris.js"] [93 "+repo:quilljs/quill"] [6 "+repo:baconjs/bacon.js"] [515 "+repo:codecombat/codecombat"] [0 "+repo:ichord/At.js"] [4 "+repo:rails/turbolinks"] [3 "+repo:koenbok/Framer"] [0 "+repo:bokeh/bokeh"] [24 "+repo:chaplinjs/chaplin"] [105 "+repo:sorich87/bootstrap-tour"] [0 "+repo:stripe/jquery.payment"] [7 "+repo:dmauro/Keypress"] [1 "+repo:Morhaus/dispatch-proxy"] [15 "+repo:FelisCatus/SwitchyOmega"] [0 "+repo:jnordberg/wintersmith"] [0 "+repo:dmotz/oriDomi"] [6 "+repo:sroze/ngInfiniteScroll"] [123 "+repo:angular-ui/angular-google-maps"] [27 "+repo:michaelficarra/CoffeeScriptRedux"] [0 "+repo:McPants/jquery.shapeshift"] [0 "+repo:HubSpot/BuckyClient"] [1 "+repo:oauth-io/oauthd"] [0 "+repo:dropbox/dropbox-js"] [72 "+repo:jenius/roots"] [4 "+repo:serkanyersen/ifvisible.js"] [24 "+repo:linemanjs/lineman"] [0 "+repo:oskarkrawczyk/heyoffline.js"] [132 "+repo:atom/vim-mode"] [0 "+repo:jaymedavis/hubble"] [0 "+repo:easelinc/tourist"] [0 "+repo:soulwire/Coffee-Physics"] [7 "+repo:stevenschobert/instafeed.js"] [27 "+repo:n3-charts/line-chart"] [0 "+repo:basecamp/local_time"] [13 "+repo:slackhq/hubot-slack"] [10 "+repo:cozy/cozy-setup"] [1 "+repo:leeluolee/puer"] [10 "+repo:shadowsocks/shadowsocks-nodejs"] [0 "+repo:shadowsocks/shadowsocks-gui"] [1 "+repo:kossnocorp/jquery.turbolinks"] [2 "+repo:passy/angular-masonry"] [0 "+repo:techpines/bone.io"] [3 "+repo:alekseykulikov/backbone-offline"] [0 "+repo:tomdionysus/foaas"] [0 "+repo:fynyky/reactor.js"] [0 "+repo:nathanboktae/mocha-phantomjs"] [0 "+repo:CaryLandholt/AngularFun"] [9 "+repo:LeanMeanFightingMachine/dploy"] [0 "+repo:audiocogs/aurora.js"] [0 "+repo:rs/pushd"] [0 "+repo:octoblu/meshblu"] [18 "+repo:gterrono/houston"] [5 "+repo:Addepar/ember-charts"] [4 "+repo:jh3y/tyto"] [0 "+repo:sstephenson/xipd"] [2 "+repo:buttercoin/buttercoin"] [0 "+repo:pyalot/webgl-heatmap"] [6 "+repo:atom/space-pen"] [0 "+repo:twilio/BankersBox"] [0 "+repo:weixiyen/messenger.js"] [0 "+repo:HubSpot/signet"] [5 "+repo:coffeedoc/codo"] [0 "+repo:rubyjs/core-lib"] [1 "+repo:elving/swag"] [22 "+repo:apiaryio/dredd"] [4 "+repo:akhodakivskiy/VimFx"] [0 "+repo:maxwells/bootstrap-tags"] [0 "+repo:localytics/angular-chosen"] [2 "+repo:outsideris/popularconvention"] [5 "+repo:Zaku-eu/colourco.de"] [9 "+repo:CaffeinatedCode/nitro"] [0 "+repo:franzenzenhofer/box2d-jquery"] [0 "+repo:nickperkinslondon/angular-bootstrap-nav-tree"] [16 "+repo:byhestia/springseed"] [28 "+repo:mojotech/pioneer"] [5 "+repo:domenic/chai-as-promised"] [0 "+repo:harvesthq/harvey"] [3 "+repo:HubSpot/humanize"] [1 "+repo:jashkenas/journo"] [0 "+repo:JoelBesada/pasteboard"] [0 "+repo:soundcloud/waveformjs"] [67 "+repo:atom/apm"] [0 "+repo:filearts/plunker"] [0 "+repo:davidgtonge/backbone_query"] [0 "+repo:mattinsler/longjohn"] [0 "+repo:domenic/sinon-chai"] [91 "+repo:qq99/echoplexus"] [0 "+repo:xhan/qqbot"] [5 "+repo:adamalbrecht/ngQuickDate"] [4 "+repo:kelp404/angular-form-builder"] [4 "+repo:github/task_list"] [12 "+repo:patriksimek/node-mssql"] [7 "+repo:for-GET/http-decision-diagram"] [1 "+repo:techpines/asset-rack"] [0 "+repo:jed/domo"] [1 "+repo:mizzao/meteor-user-status"] [0 "+repo:ricardobeat/filr"] [0 "+repo:nkohari/jwalk"] [0 "+repo:zmoazeni/gitspective"]) '([0 "+repo:PredictionIO/PredictionIO"] [149 "+repo:takezoe/gitbucket"] [0 "+repo:lhartikk/ArnoldC"] [7 "+repo:twitter/scalding"] [39 "+repo:snowplow/snowplow"] [362 "+repo:scala-js/scala-js"] [0 "+repo:MojoJolo/textteaser"] [0 "+repo:pocorall/scaloid"] [215 "+repo:mesosphere/marathon"] [0 "+repo:fpinscala/fpinscala"] [11 "+repo:twitter/summingbird"] [1 "+repo:rtyley/bfg-repo-cleaner"] [53 "+repo:ornicar/lila"] [0 "+repo:twitter/iago"] [0 "+repo:amplab/shark"] [2 "+repo:twitter/algebird"] [15 "+repo:laurilehmijoki/s3_website"] [0 "+repo:twitter/finatra"] [1 "+repo:twitter/cassovary"] [0 "+repo:eligosource/eventsourced"] [0 "+repo:lihaoyi/Metascala"] [5 "+repo:mauricio/postgresql-async"] [27 "+repo:scala/pickling"] [3 "+repo:ReactiveMongo/ReactiveMongo"] [0 "+repo:lihaoyi/scala.rx"] [13 "+repo:scala/async"] [1 "+repo:playframework/play-slick"] [0 "+repo:t2v/play2-auth"] [3 "+repo:json4s/json4s"] [0 "+repo:nscala-time/nscala-time"] [73 "+repo:skinny-framework/skinny-framework"] [27 "+repo:kamon-io/Kamon"] [0 "+repo:vkostyukov/scalacaster"] [3 "+repo:etaty/rediscala"] [23 "+repo:play2war/play2-war-plugin"] [1 "+repo:sksamuel/elastic4s"] [31 "+repo:lampepfl/dotty"] [6 "+repo:dbpedia-spotlight/dbpedia-spotlight"] [0 "+repo:BIDData/BIDMach"] [22 "+repo:scalaz/scalaz-stream"] [26 "+repo:puffnfresh/wartremover"] [0 "+repo:foursquare/twofishes"] [0 "+repo:twitter/twitter-server"] [0 "+repo:pfn/android-sdk-plugin"] [0 "+repo:adamw/macwire"] [4 "+repo:ReactiveMongo/Play-ReactiveMongo"] [0 "+repo:Eliah-Lakhin/papa-carlo"] [9 "+repo:sirthias/parboiled2"] [0 "+repo:factorie/factorie"] [52 "+repo:typesafehub/activator"] [9 "+repo:mohiva/play-silhouette"] [4 "+repo:twitter/storehaus"] [7 "+repo:tumblr/collins"] [0 "+repo:lihaoyi/scalatags"] [2 "+repo:twitter/bijection"] [3 "+repo:scodec/scodec"] [1 "+repo:matthiasn/sse-chat"] [43 "+repo:bigdatagenomics/adam"] [0 "+repo:Netflix/edda"] [1 "+repo:twitter/chill"] [6 "+repo:MoeOrganization/moe"] [2 "+repo:argonaut-io/argonaut"] [13 "+repo:tminglei/slick-pg"] [1 "+repo:macroid/macroid"] [8 "+repo:websudos/phantom"] [21 "+repo:http4s/http4s"] [0 "+repo:scalatest/scalatest"] [4 "+repo:seratch/AWScala"] [5 "+repo:tpolecat/doobie"] [0 "+repo:twitter/cassie"] [5 "+repo:Comcast/sirius"] [18 "+repo:MrTJP/ProjectRed"] [0 "+repo:mashupbots/socko"] [0 "+repo:MightyPirates/OpenComputers"] [1 "+repo:sksamuel/scrimage"] [2 "+repo:aselab/scala-activerecord"] [0 "+repo:nulab/scala-oauth2-provider"] [10 "+repo:twitter/scala_school2"] [7 "+repo:amplab/graphx"] [2 "+repo:scalanlp/nak"] [0 "+repo:leon/play-salat"] [0 "+repo:sbt/sbt-web"] [0 "+repo:ucb-bar/chisel"] [7 "+repo:stackmob/newman"] [0 "+repo:headinthebox/CourseraCodeSamplesReactiveProgramming"] [61 "+repo:p2t2/figaro"] [9 "+repo:mattpap/IScala"] [0 "+repo:spray/twirl"] [0 "+repo:heathermiller/progfun-stats"] [0 "+repo:joa/apparat"] [1 "+repo:playframework/twirl"] [1 "+repo:tototoshi/scala-csv"] [0 "+repo:nau/jscala"] [0 "+repo:scalameter/scalameter"] [35 "+repo:Coinffeine/coinffeine"] [4 "+repo:typesafehub/zinc"] [0 "+repo:playforscala/sample-applications"] [6 "+repo:scoverage/scalac-scoverage-plugin"] [0 "+repo:typesafehub/scalalogging"] [1 "+repo:erikvanoosten/metrics-scala"]) '([0 "+repo:yiisoft/yii2"] [0 "+repo:Automattic/_s"] [48 "+repo:serbanghita/Mobile-Detect"] [20 "+repo:sovereign/sovereign"] [0 "+repo:yiisoft/yii"] [0 "+repo:PHPOffice/PHPExcel"] [354 "+repo:laravel/framework"] [12 "+repo:justinwalsh/daux.io"] [4858 "+repo:owncloud/core"] [0 "+repo:subtlepatterns/SubtlePatterns"] [8 "+repo:reactphp/react"] [0 "+repo:briannesbitt/Carbon"] [0 "+repo:JeffreyWay/Laravel-4-Generators"] [99 "+repo:octobercms/october"] [21 "+repo:google/google-api-php-client"] [7 "+repo:Intervention/image"] [14 "+repo:ratchetphp/Ratchet"] [170 "+repo:FriendsOfPHP/PHP-CS-Fixer"] [2 "+repo:thephpleague/flysystem"] [21 "+repo:filp/whoops"] [0 "+repo:erusev/parsedown"] [51 "+repo:pattern-lab/patternlab-php"] [313 "+repo:WP-API/WP-API"] [0 "+repo:barryvdh/laravel-ide-helper"] [332 "+repo:bolt/bolt"] [0 "+repo:barryvdh/laravel-debugbar"] [0 "+repo:picocms/Pico"] [0 "+repo:umpirsky/country-list"] [11 "+repo:fex-team/fis"] [42 "+repo:dompdf/dompdf"] [2 "+repo:mledoze/countries"] [46 "+repo:electerious/Lychee"] [0 "+repo:roots/bedrock"] [48 "+repo:YOURLS/YOURLS"] [0 "+repo:TechEmpower/FrameworkBenchmarks"] [0 "+repo:rmccue/Requests"] [12 "+repo:jarednova/timber"] [10 "+repo:Zizaco/entrust"] [0 "+repo:dodgepudding/wechat-php-sdk"] [0 "+repo:rocketeers/rocketeer"] [31 "+repo:aws/aws-sdk-php"] [22 "+repo:thephpleague/oauth2-server"] [2 "+repo:panique/huge"] [107 "+repo:bcosca/fatfree"] [0 "+repo:cartalyst/sentry"] [0 "+repo:ccoenraets/backbone-directory"] [55 "+repo:Circa75/dropplets"] [0 "+repo:themeskult/wp-svbtle"] [0 "+repo:ircmaxell/password_compat"] [9 "+repo:aheinze/cockpit"] [3 "+repo:typecho/typecho"] [0 "+repo:roundcube/roundcubemail"] [0 "+repo:catfan/Medoo"] [0 "+repo:markjaquith/WordPress-Skeleton"] [277 "+repo:wallabag/wallabag"] [0 "+repo:vlucas/phpdotenv"] [0 "+repo:bshaffer/oauth2-server-php"] [2 "+repo:JosephLenton/PHP-Error"] [0 "+repo:opauth/opauth"] [1 "+repo:twittem/wp-bootstrap-navwalker"] [91 "+repo:FrozenNode/Laravel-Administrator"] [0 "+repo:phpmyadmin/phpmyadmin"] [0 "+repo:PrestaShop/PrestaShop"] [8 "+repo:Zizaco/confide"] [52 "+repo:deployphp/deployer"] [2 "+repo:Maatwebsite/Laravel-Excel"] [17 "+repo:Codiad/Codiad"] [0 "+repo:drush-ops/drush"] [14 "+repo:jenssegers/laravel-mongodb"] [2 "+repo:walkor/Workerman"] [0 "+repo:raveren/kint"] [0 "+repo:PHPOffice/PHPWord"] [0 "+repo:maximebf/php-debugbar"] [71 "+repo:phalcon/zephir"] [0 "+repo:markjaquith/WP-Stack"] [0 "+repo:laravelbook/ardent"] [3 "+repo:J7mbo/twitter-api-php"] [0 "+repo:thephpleague/fractal"] [10 "+repo:lucadegasperi/oauth2-server-laravel"] [0 "+repo:elliotcondon/acf"] [6 "+repo:giggsey/libphonenumber-for-php"] [45 "+repo:hwi/HWIOAuthBundle"] [0 "+repo:danielstjules/Stringy"] [0 "+repo:toddmotto/html5blank"] [123 "+repo:ampache/ampache"] [0 "+repo:bastianallgeier/kirbycms"] [28 "+repo:RainLoop/rainloop-webmail"] [0 "+repo:VerbalExpressions/PHPVerbalExpressions"] [16 "+repo:bobthecow/psysh"] [0 "+repo:komarserjio/notejam"] [0 "+repo:lucb1e/cookielesscookies"] [3 "+repo:ramsey/uuid"] [36 "+repo:formers/former"] [2 "+repo:brandonwamboldt/utilphp"] [0 "+repo:FriendsOfPHP/Sami"] [18 "+repo:reduxframework/redux-framework"] [0 "+repo:dmolsen/Detector"] [0 "+repo:thephpleague/oauth2-client"] [0 "+repo:marcoarment/secondcrack"] [7 "+repo:nelmio/alice"]) '([353 "+repo:docker/docker"] [295 "+repo:syncthing/syncthing"] [4 "+repo:go-martini/martini"] [0 "+repo:astaxie/build-web-application-with-golang"] [214 "+repo:coreos/etcd"] [6 "+repo:joewalnes/websocketd"] [83 "+repo:influxdb/influxdb"] [13 "+repo:inconshreveable/ngrok"] [171 "+repo:bitly/nsq"] [168 "+repo:spf13/hugo"] [8 "+repo:astaxie/beego"] [380 "+repo:mitchellh/packer"] [90 "+repo:hashicorp/consul"] [215 "+repo:flynn/flynn"] [0 "+repo:golang/groupcache"] [26 "+repo:shipyard/shipyard"] [3 "+repo:buger/gor"] [15 "+repo:hashicorp/serf"] [10 "+repo:gopherjs/gopherjs"] [4 "+repo:codegangsta/cli"] [13 "+repo:burke/zeus"] [1 "+repo:youtube/vitess"] [34 "+repo:prometheus/prometheus"] [7 "+repo:boltdb/bolt"] [1 "+repo:tsenart/vegeta"] [20 "+repo:tools/godep"] [41 "+repo:mozilla-services/heka"] [0 "+repo:igrigorik/ga-beacon"] [0 "+repo:mjibson/goread"] [1 "+repo:jinzhu/gorm"] [9 "+repo:pksunkara/alpaca"] [25 "+repo:cyfdecyf/cow"] [27 "+repo:github/git-lfs"] [27 "+repo:junegunn/fzf"] [3 "+repo:sjwhitworth/golearn"] [6 "+repo:julienschmidt/httprouter"] [159 "+repo:coreos/fleet"] [14 "+repo:kelseyhightower/confd"] [0 "+repo:ant0ine/go-json-rest"] [16 "+repo:go-gorp/gorp"] [0 "+repo:facebookgo/grace"] [5 "+repo:robertkrimen/otto"] [0 "+repo:smartystreets/goconvey"] [1 "+repo:Unknwon/the-way-to-go_ZH_CN"] [33 "+repo:go-sql-driver/mysql"] [13 "+repo:lib/pq"] [169 "+repo:tsuru/tsuru"] [0 "+repo:garyburd/redigo"] [0 "+repo:gorilla/mux"] [2 "+repo:Sirupsen/logrus"] [0 "+repo:go-qml/qml"] [2 "+repo:hybridgroup/gobot"] [6 "+repo:elastic/logstash-forwarder"] [12 "+repo:goraft/raft"] [6 "+repo:HouzuoGuo/tiedot"] [0 "+repo:gorilla/websocket"] [5 "+repo:elves/elvish"] [82 "+repo:nytlabs/streamtools"] [0 "+repo:Unknwon/go-fundamental-programming"] [0 "+repo:benmanns/goworker"] [0 "+repo:google/go-github"] [1 "+repo:emicklei/go-restful"] [0 "+repo:stretchr/testify"] [1 "+repo:mitchellh/gox"] [1 "+repo:nats-io/gnatsd"] [0 "+repo:codegangsta/gin"] [0 "+repo:mmcgrana/gobyexample"] [4 "+repo:jmoiron/sqlx"] [2 "+repo:laher/goxc"] [8 "+repo:nsf/termbox-go"] [1 "+repo:howeyc/fsnotify"] [0 "+repo:cdarwin/go-koans"] [193 "+repo:bosun-monitor/bosun"] [0 "+repo:spf13/cobra"] [5 "+repo:golang/lint"] [0 "+repo:astaxie/Go-in-Action"] [1 "+repo:bitly/go-simplejson"] [0 "+repo:Terry-Mao/gopush-cluster"] [0 "+repo:mattn/gom"] [0 "+repo:rcrowley/go-tigertonic"] [0 "+repo:cloudflare/redoctober"] [0 "+repo:bmizerany/pat"] [1 "+repo:huichen/wukong"] [56 "+repo:smira/aptly"] [0 "+repo:xiam/hyperfox"] [0 "+repo:polaris1119/The-Golang-Standard-Library-by-Example"] [0 "+repo:elazarl/goproxy"] [2 "+repo:ChimeraCoder/gojson"] [0 "+repo:gocraft/web"] [0 "+repo:jingweno/gh"] [29 "+repo:go-xorm/xorm"] [0 "+repo:golang/glog"] [20 "+repo:heroku/hk"] [5 "+repo:nfnt/resize"] [0 "+repo:jondot/groundcontrol"] [0 "+repo:mindreframer/golang-stuff"] [31 "+repo:syndtr/goleveldb"] [20 "+repo:onsi/ginkgo"] [35 "+repo:oleiade/trousseau"] [0 "+repo:PuerkitoBio/gocrawl"]) '([0 "+repo:jfeinstein10/SlidingMenu"] [261 "+repo:libgdx/libgdx"] [0 "+repo:square/picasso"] [143 "+repo:ReactiveX/RxJava"] [0 "+repo:excilys/androidannotations"] [4 "+repo:greenrobot/EventBus"] [136 "+repo:square/okhttp"] [0 "+repo:JakeWharton/butterknife"] [7 "+repo:Bearded-Hen/Android-Bootstrap"] [1 "+repo:EnterpriseQualityCoding/FizzBuzzEnterpriseEdition"] [0 "+repo:nhaarman/ListViewAnimations"] [3 "+repo:chrisbanes/PhotoView"] [0 "+repo:astuetz/PagerSlidingTabStrip"] [1 "+repo:AndroidBootstrap/android-bootstrap"] [10 "+repo:square/dagger"] [11 "+repo:Netflix/Hystrix"] [61 "+repo:facebook/presto"] [107 "+repo:bumptech/glide"] [28 "+repo:gabrielemariotti/cardslib"] [17 "+repo:etsy/AndroidStaggeredGrid"] [4 "+repo:koush/ion"] [19 "+repo:umano/AndroidSlidingUpPanel"] [0 "+repo:LMAX-Exchange/disruptor"] [7 "+repo:roboguice/roboguice"] [0 "+repo:chrisbanes/ActionBar-PullToRefresh"] [5 "+repo:47deg/android-swipelistview"] [81 "+repo:OpenRefine/OpenRefine"] [37 "+repo:emilsjolander/StickyListHeaders"] [0 "+repo:Netflix/SimianArmy"] [0 "+repo:square/otto"] [107 "+repo:thinkaurelius/titan"] [0 "+repo:amlcurran/ShowcaseView"] [6 "+repo:lucasr/twoway-view"] [0 "+repo:pardom/ActiveAndroid"] [0 "+repo:wyouflf/xUtils"] [0 "+repo:springside/springside4"] [14 "+repo:keyboardsurfer/Crouton"] [4 "+repo:koush/AndroidAsync"] [0 "+repo:twitter/zipkin"] [54 "+repo:stephanenicolas/robospice"] [399 "+repo:spring-projects/spring-boot"] [88 "+repo:jhipster/generator-jhipster"] [1 "+repo:castorflex/SmoothProgressBar"] [0 "+repo:SimonVT/android-menudrawer"] [0 "+repo:ManuelPeinado/FadingActionBar"] [0 "+repo:bauerca/drag-sort-listview"] [1 "+repo:facebook/rebound"] [0 "+repo:Prototik/HoloEverywhere"] [3 "+repo:chrisjenx/Calligraphy"] [0 "+repo:apache/storm"] [0 "+repo:Trinea/android-common"] [4 "+repo:square/android-times-square"] [7 "+repo:ACRA/acra"] [0 "+repo:mcxiaoke/android-volley"] [75 "+repo:realm/realm-java"] [0 "+repo:yangfuhai/afinal"] [0 "+repo:JakeWharton/u2020"] [5 "+repo:Comcast/FreeFlow"] [3 "+repo:alibaba/dubbo"] [0 "+repo:processing/processing"] [5 "+repo:openaphid/android-flip"] [983 "+repo:orientechnologies/orientdb"] [95 "+repo:druid-io/druid"] [0 "+repo:JakeWharton/hugo"] [0 "+repo:JakeWharton/DiskLruCache"] [0 "+repo:vinc3m1/RoundedImageView"] [340 "+repo:neo4j/neo4j"] [17 "+repo:qii/weiciyuan"] [0 "+repo:h2oai/h2o-2"] [0 "+repo:JetBrains/kotlin"] [2 "+repo:path/android-priority-jobqueue"] [0 "+repo:avast/android-styled-dialogs"] [4 "+repo:yinwang0/pysonar2"] [2 "+repo:jgilfelt/SystemBarTint"] [198 "+repo:gocd/gocd"] [1 "+repo:yixia/VitamioBundle"] [40 "+repo:reactor/reactor"] [68 "+repo:brettwooldridge/HikariCP"] [6 "+repo:SpecialCyCi/AndroidResideMenu"] [54 "+repo:koral--/android-gif-drawable"] [1 "+repo:derekbrameyer/android-betterpickers"] [0 "+repo:jfeinstein10/JazzyViewPager"] [0 "+repo:tjerkw/Android-SlideExpandableListView"] [161 "+repo:jankotek/mapdb"] [0 "+repo:google/auto"] [652 "+repo:openhab/openhab"] [0 "+repo:ikew0ng/SwipeBackLayout"] [33 "+repo:facebook/buck"] [69 "+repo:mybatis/mybatis-3"] [0 "+repo:amplab/tachyon"] [0 "+repo:Todd-Davies/ProgressWheel"] [4 "+repo:Bilibili/DanmakuFlameMaster"] [119 "+repo:SecUpwN/Android-IMSI-Catcher-Detector"] [0 "+repo:videlalvaro/gifsockets"] [235 "+repo:robovm/robovm"] [0 "+repo:kikoso/android-stackblur"] [5 "+repo:beworker/pinned-section-listview"] [3 "+repo:JohnPersano/SuperToasts"] [0 "+repo:Netflix/ice"] [0 "+repo:flavienlaurent/NotBoringActionBar"]) '([0 "+repo:koalaman/shellcheck"] [36 "+repo:elm-lang/elm-compiler"] [222 "+repo:purescript/purescript"] [23 "+repo:faylang/fay"] [9 "+repo:gibiansky/IHaskell"] [0 "+repo:valderman/haste-compiler"] [11 "+repo:ekmett/lens"] [0 "+repo:awgn/cgrep"] [0 "+repo:mikeizbicki/HLearn"] [0 "+repo:xmonad/osxmonad"] [194 "+repo:haskell/cabal"] [0 "+repo:GaloisInc/HaLVM"] [0 "+repo:joeyh/github-backup"] [0 "+repo:chrisdone/hell"] [5 "+repo:aurapm/aura"] [11 "+repo:switchface/helm"] [8 "+repo:egison/egison"] [0 "+repo:sdiehl/kaleidoscope"] [0 "+repo:davidbrewer/xmonad-ubuntu-conf"] [3 "+repo:ndmitchell/shake"] [0 "+repo:huangz1990/real-world-haskell-cn"] [1 "+repo:haskoin/haskoin"] [0 "+repo:Gabriel439/Haskell-Pipes-Library"] [3 "+repo:jaspervdj/stylish-haskell"] [1 "+repo:ndmitchell/hlint"] [0 "+repo:yesodweb/Shelly.hs"] [0 "+repo:agrafix/Spock"] [2 "+repo:pcapriotti/optparse-applicative"] [0 "+repo:kqr/gists"] [0 "+repo:anton-k/ru-haskell-book"] [21 "+repo:clash-lang/clash-compiler"] [0 "+repo:sebastiaanvisser/clay"] [0 "+repo:HaskVan/HaskellKoans"] [0 "+repo:bitc/hdevtools"] [0 "+repo:haskell/haskell-platform"] [1 "+repo:ekmett/machines"] [0 "+repo:sweirich/pi-forall"] [0 "+repo:chris-taylor/aima-haskell"] [15 "+repo:haskell/hackage-server"] [0 "+repo:nick8325/quickcheck"] [0 "+repo:ndmitchell/hoogle"] [53 "+repo:ucsd-progsys/liquidhaskell"] [0 "+repo:bos/critbit"] [0 "+repo:timbod7/haskell-chart"] [4 "+repo:prowdsponsor/esqueleto"] [11 "+repo:ajhc/ajhc"] [125 "+repo:simonmichael/hledger"] [1 "+repo:feuerbach/tasty"] [4 "+repo:snoyberg/keter"] [1 "+repo:VinylRecords/Vinyl"] [0 "+repo:w7cook/AoPL"] [0 "+repo:corsis/PortFusion"] [0 "+repo:tibbe/cassava"] [0 "+repo:simonmar/async"] [1 "+repo:acid-state/acid-state"] [8 "+repo:UU-ComputerScience/uhc"] [0 "+repo:benl23x5/gloss"] [3 "+repo:bscarlet/llvm-general"] [61 "+repo:haskell/c2hs"] [0 "+repo:noelmarkham/learn-you-a-haskell-exercises"] [1 "+repo:snapframework/io-streams"] [0 "+repo:simonmar/parconc-examples"] [37 "+repo:haskell-suite/haskell-src-exts"] [0 "+repo:jwiegley/gitlib"] [0 "+repo:entropia/tip-toi-reveng"] [5 "+repo:lambdabot/lambdabot"] [1 "+repo:Lemmih/hsSDL2"] [5 "+repo:kallisti-dev/hs-webdriver"] [2 "+repo:Daniel-Diaz/HaTeX"] [0 "+repo:snoyberg/http-conduit"] [1 "+repo:gtk2hs/gtk2hs"] [0 "+repo:prowdsponsor/fb"] [0 "+repo:snoyberg/http-client"] [3 "+repo:haskell-distributed/distributed-process-platform"] [11 "+repo:afcowie/http-streams"] [1 "+repo:haskell/vector"] [0 "+repo:AccelerateHS/accelerate-cuda"] [0 "+repo:simonmar/happy"] [0 "+repo:cyga/real-world-haskell"] [0 "+repo:haskell/bytestring"] [0 "+repo:ocharles/blog"] [0 "+repo:mightybyte/snaplet-postgresql-simple"] [1 "+repo:freizl/hoauth2"] [12 "+repo:diagrams/diagrams-lib"] [1 "+repo:diagrams/diagrams-core"] [1 "+repo:ekmett/parsers"] [0 "+repo:chrisdone/nm"] [0 "+repo:qrilka/xlsx"] [0 "+repo:Gabriel439/Haskell-Foldl-Library"] [0 "+repo:yesodweb/yesod-scaffold"] [1 "+repo:thoughtbot/yesod-auth-oauth2"] [0 "+repo:andreyLevushkin/LambdaWars"] [0 "+repo:raaz-crypto/raaz"] [0 "+repo:himura/twitter-types"] [4 "+repo:diagrams/diagrams-contrib"] [0 "+repo:oxij/haskell-course-ru"] [0 "+repo:fernandocastor/skimscheme"] [0 "+repo:Duke-PL-Course/Assignments"]) '([0 "+repo:discourse/discourse"] [1 "+repo:cantino/huginn"] [88 "+repo:caskroom/homebrew-cask"] [12 "+repo:Thibaut/devdocs"] [0 "+repo:charliesome/better_errors"] [4 "+repo:mperham/sidekiq"] [113 "+repo:bbatsov/rubocop"] [0 "+repo:rails-api/rails-api"] [0 "+repo:mame/quine-relay"] [0 "+repo:thoughtbot/guides"] [10 "+repo:square/maximum-awesome"] [0 "+repo:elabs/pundit"] [18 "+repo:BBC-News/wraith"] [20 "+repo:zmoazeni/csscss"] [0 "+repo:lockitron/selfstarter"] [2 "+repo:layervault/psd.rb"] [21 "+repo:FontCustom/fontcustom"] [0 "+repo:nomad/shenzhen"] [3 "+repo:mina-deploy/mina"] [1 "+repo:bkeepers/dotenv"] [0 "+repo:RailsApps/rails-composer"] [1 "+repo:nomad/cupertino"] [11 "+repo:dejan/rails_panel"] [38 "+repo:swanson/stringer"] [3 "+repo:laserlemon/figaro"] [14 "+repo:boxen/our-boxen"] [1 "+repo:peek/peek"] [0 "+repo:ankane/searchkick"] [18 "+repo:lotus/lotus"] [0 "+repo:kickstarter/rack-attack"] [28 "+repo:rails/spring"] [1 "+repo:tldr-pages/tldr"] [3 "+repo:mitchellh/vagrant-aws"] [145 "+repo:brigade/scss-lint"] [5 "+repo:Homebrew/homebrew-php"] [0 "+repo:apotonick/reform"] [0 "+repo:nomad/houston"] [11 "+repo:deivid-rodriguez/byebug"] [3 "+repo:boxen/boxen"] [54 "+repo:Homebrew/linuxbrew"] [0 "+repo:natew/obtvse"] [0 "+repo:jch/html-pipeline"] [37 "+repo:ruby-concurrency/concurrent-ruby"] [0 "+repo:brandonhilkert/sucker_punch"] [0 "+repo:maccman/monocle"] [2 "+repo:ptwobrussell/Mining-the-Social-Web-2nd-Edition"] [0 "+repo:jewel/clearskies"] [0 "+repo:paulasmuth/recommendify"] [0 "+repo:feedbin/feedbin"] [0 "+repo:redmine/redmine"] [2 "+repo:twitter/secureheaders"] [0 "+repo:jubos/fake-s3"] [0 "+repo:propublica/upton"] [2 "+repo:ankane/groupdate"] [7 "+repo:gitlabhq/gitlab-ci"] [3 "+repo:smdahlen/vagrant-digitalocean"] [1 "+repo:desktoppr/wbench"] [30 "+repo:rails-assets/rails-assets"] [39 "+repo:thoughtbot/gitsh"] [19 "+repo:trulia/hologram"] [5 "+repo:websocket-rails/websocket-rails"] [1 "+repo:krisleech/wisper"] [1 "+repo:emberjs/ember-rails"] [6 "+repo:roidrage/lograge"] [8 "+repo:pearkes/tugboat"] [0 "+repo:evrone/quiet_assets"] [0 "+repo:apotonick/trailblazer"] [0 "+repo:square/cane"] [1 "+repo:steelThread/redmon"] [0 "+repo:twitter/activerecord-reputation-system"] [2 "+repo:rubymotion/BubbleWrap"] [17 "+repo:elastic/elasticsearch-rails"] [0 "+repo:serverspec/serverspec"] [85 "+repo:clearsightstudio/ProMotion"] [0 "+repo:maccman/abba"] [0 "+repo:rails/strong_parameters"] [0 "+repo:MiniProfiler/rack-mini-profiler"] [4 "+repo:supermarin/xcpretty"] [7 "+repo:remiprev/her"] [1 "+repo:nathanl/authority"] [5 "+repo:interagent/prmd"] [4 "+repo:gottfrois/dashing-rails"] [0 "+repo:hybridgroup/artoo"] [2 "+repo:rharriso/bower-rails"] [0 "+repo:pluralsight/git-internals-pdf"] [0 "+repo:chrismccord/sync"] [11 "+repo:wpscanteam/wpscan"] [2 "+repo:grobie/soundcloud2000"] [8 "+repo:thoughtbot/liftoff"] [1 "+repo:collectiveidea/interactor"] [3 "+repo:HipByte/RubyMotionSamples"] [0 "+repo:jondot/sneakers"] [1 "+repo:stripe/einhorn"] [0 "+repo:davidcelis/recommendable"] [73 "+repo:calabash/calabash-ios"] [0 "+repo:ndbroadbent/turbo-sprockets-rails3"] [0 "+repo:Shopify/identity_cache"] [0 "+repo:RailsApps/rails-stripe-membership-saas"] [0 "+repo:jneen/rouge"] [0 "+repo:louismullie/treat"]) '([41 "+repo:jkbrzt/httpie"] [0 "+repo:django/django"] [0 "+repo:ansible/ansible"] [20 "+repo:shadowsocks/shadowsocks"] [1 "+repo:CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers"] [5 "+repo:Valloric/YouCompleteMe"] [57 "+repo:docker/compose"] [0 "+repo:faif/python-patterns"] [507 "+repo:deis/deis"] [0 "+repo:powerline/powerline"] [0 "+repo:bup/bup"] [23 "+repo:mail-in-a-box/mailinabox"] [0 "+repo:facebookarchive/huxley"] [0 "+repo:docopt/docopt"] [257 "+repo:sqlmapproject/sqlmap"] [0 "+repo:chriskiehl/Gooey"] [4 "+repo:fxsjy/jieba"] [2 "+repo:numenta/nupic"] [4 "+repo:joke2k/faker"] [0 "+repo:gleitz/howdoi"] [4 "+repo:phusion/baseimage-docker"] [0 "+repo:lra/mackup"] [194 "+repo:aws/aws-cli"] [0 "+repo:amoffat/sh"] [4 "+repo:spotify/luigi"] [1 "+repo:jisaacks/GitGutter"] [0 "+repo:harelba/q"] [1 "+repo:Katee/quietnet"] [0 "+repo:idank/explainshell"] [0 "+repo:mahmoud/boltons"] [51 "+repo:docker/docker-registry"] [7 "+repo:sloria/TextBlob"] [5 "+repo:jorgebastida/glue"] [46 "+repo:nicolaiarocci/eve"] [0 "+repo:codelucas/newspaper"] [16 "+repo:crsmithdev/arrow"] [0 "+repo:fogleman/Minecraft"] [16 "+repo:quantopian/zipline"] [41 "+repo:MobileChromeApps/mobile-chrome-apps"] [32 "+repo:mwaskom/seaborn"] [2 "+repo:sivel/speedtest-cli"] [0 "+repo:redecentralize/alternative-internet"] [47 "+repo:tgalal/yowsup"] [12 "+repo:yhat/ggplot"] [5 "+repo:gevent/gevent"] [5 "+repo:emre/storm"] [0 "+repo:thearn/webcam-pulse-detector"] [0 "+repo:taobao/nginx-book"] [0 "+repo:edx/edx-platform"] [6 "+repo:jeffknupp/sandman"] [19 "+repo:AppScale/appscale"] [14 "+repo:taigaio/taiga-back"] [33 "+repo:audreyr/cookiecutter"] [11 "+repo:aziz/PlainTasks"] [0 "+repo:milkbikis/powerline-shell"] [53 "+repo:falconry/falcon"] [88 "+repo:python-pillow/Pillow"] [0 "+repo:omab/python-social-auth"] [1 "+repo:chrisallenlane/cheat"] [0 "+repo:worldveil/dejavu"] [9 "+repo:lihaoyi/macropy"] [0 "+repo:djaiss/mapsicon"] [21 "+repo:maebert/jrnl"] [4 "+repo:Zulko/moviepy"] [243 "+repo:Tribler/tribler"] [0 "+repo:martinblech/xmltodict"] [2 "+repo:kachayev/fn.py"] [127 "+repo:davidhalter/jedi"] [0 "+repo:amscanne/huptime"] [0 "+repo:tschellenbach/Stream-Framework"] [13 "+repo:Bitmessage/PyBitmessage"] [1 "+repo:pudo/dataset"] [1 "+repo:iBaa/PlexConnect"] [0 "+repo:gelstudios/gitfiti"] [110 "+repo:numba/numba"] [0 "+repo:etsy/skyline"] [55 "+repo:freedomofpress/securedrop"] [0 "+repo:ajalt/fuckitpy"] [0 "+repo:0rpc/zerorpc-python"] [2 "+repo:wrobstory/vincent"] [0 "+repo:JakeWharton/pidcat"] [20 "+repo:flask-restful/flask-restful"] [12 "+repo:revolunet/sublimetext-markdown-preview"] [0 "+repo:douban/dpark"] [1 "+repo:DanMcInerney/LANs.py"] [51 "+repo:soimort/you-get"] [4 "+repo:litl/rauth"] [0 "+repo:TooTallNate/node-gyp"] [3 "+repo:sshwsfc/django-xadmin"] [0 "+repo:gurgeh/selfspy"] [0 "+repo:kovidgoyal/calibre"] [4 "+repo:simon-weber/gmusicapi"] [1 "+repo:fivesheep/chnroutes"] [0 "+repo:mher/flower"] [3 "+repo:flask-admin/flask-admin"] [0 "+repo:posativ/isso"] [26 "+repo:qianlifeng/Wox"] [1 "+repo:mhagger/git-imerge"] [0 "+repo:VitaliyRodnenko/geeknote"] [2 "+repo:huhamhire/huhamhire-hosts"])))
