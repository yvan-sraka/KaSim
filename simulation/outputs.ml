type flux_data = {
    flux_name : string;
    flux_start : float;
    flux_hits : int array;
    flux_fluxs : float array array;
  }
type flux_plot =
  { flux_rules : string array;
    flux_data : flux_data;
    flux_end : float;
  }

let create_flux env counter name =
  let size = Environment.nb_syntactic_rules env + 1 in
  {
    flux_name = name;
    flux_start = Counter.current_time counter;
    flux_hits = Array.make size 0;
    flux_fluxs = Array.make_matrix size size 0.;
  }

let incr_flux_flux of_rule on_rule v flux =
  flux.flux_fluxs.(of_rule).(on_rule) <-
    flux.flux_fluxs.(of_rule).(on_rule) +. v

let incr_flux_hit of_rule flux =
  flux.flux_hits.(of_rule) <- succ flux.flux_hits.(of_rule)

let get_flux_name flux = flux.flux_name
let flux_has_name name flux = flux.flux_name = name

let stop_flux env counter flux =
  let size = Environment.nb_syntactic_rules env + 1 in
  let flux_rules =
    Array.init size
	       (Format.asprintf "%a" (Environment.print_ast_rule ~env))
  in
  { flux_rules = flux_rules; flux_data = flux ;
    flux_end = Counter.current_time counter }

let dot_of_flux flux =
  let printer desc =
    let () = Format.fprintf
	       desc "@[<v>digraph G{ label=\"Flux map\" ; labelloc=\"t\" ; " in
    let () = Format.fprintf
	       desc "node [shape=box,style=filled,fillcolor=lightskyblue]@," in
    let () =
      Pp.array
	(fun _ -> ())
	(fun s ->
	 Pp.array
	   Pp.empty
	   (fun d f v ->
	    if v=0. then ()
	    else
	      let color,arrowhead =
		if v<0. then ("red3","tee") else ("green3","normal") in
	      Format.fprintf
		f
		"@[<h>\"%s\" -> \"%s\" [weight=%d,label=\"%.3f\",color=%s,arrowhead=%s];@]@,"
		flux.flux_rules.(s)
		flux.flux_rules.(d)
		(abs (int_of_float v)) v color arrowhead))
	desc flux.flux_data.flux_fluxs in
    Format.fprintf desc "}@]@."
  in
  Kappa_files.with_flux flux.flux_data.flux_name printer

let print_json_of_flux f flux =
  let () = Format.fprintf
	     f "@[<v>{@ \"bio_begin_time\" : %f,@ \"bio_end_time\" : %f,@ "
	     flux.flux_data.flux_start flux.flux_end in
  let () =
    Format.fprintf
      f "@[\"rules\" :@ @[[%a]@]@],@ @[\"hits\" :@ @[[%a]@]@],@ "
      (Pp.array Pp.comma (fun _ -> Format.pp_print_string)) flux.flux_rules
      (Pp.array Pp.comma (fun _ -> Format.pp_print_int)) flux.flux_data.flux_hits in
  Format.fprintf
    f "@[\"fluxs\" :@ @[[%a]@]@]@ }@]"
    (Pp.array
       Pp.comma
       (fun _ f x ->
	Format.fprintf
	  f "@[[%a]@]"
	  (Pp.array Pp.comma (fun _ f y -> Format.pp_print_float f y)) x))
    flux.flux_data.flux_fluxs

let json_of_flux flux =
  Kappa_files.with_flux
    flux.flux_data.flux_name (fun f -> print_json_of_flux f flux)

let html_of_flux flux =
  Kappa_files.with_flux
    flux.flux_data.flux_name
    (Pp_html.graph_page
       (fun f -> Format.pp_print_string f "Dynamic influence map")
       ["http://d3js.org/d3.v3.min.js"]
       (fun f ->
       let () =
	  Format.fprintf
	    f "@[<v 2><style>@,.chord path {@ fill-opacity: .67;@ " in
       Format.fprintf
	 f "stroke: #000;@ stroke-width: .5px;@ }@]@,</style>")
       (fun f ->
	let () = Format.fprintf f "@[<hv 2><form id=\"menu\">@," in
	let () = Format.fprintf f "@[<v 2><div class=\"form-group\">@," in
	let () =
	  Format.fprintf f "<label for=\"correction\">Correction</label>@," in
	let () =
	  Format.fprintf
	    f
	    "<select class=\"form-control\" id=\"correction\" onchange=\"drawDIM()\">@," in
	let () =
	  Format.fprintf f "<option value=\"none\">None</option>@," in
	let () = Format.fprintf
		   f "<option value=\"hits\">Rule occurences</option>@," in
	let () = Format.fprintf
		   f "<option value=\"time\">Time</option>@]@,</select>@,</div>@," in
	let () = Format.fprintf f "@[<v 2><label class=\"checkbox-inline\">@," in
	let () =
	  Format.fprintf
	    f
	    "<input type=\"checkbox\" onclick=\"toggleSelfInfluence()\">@," in
	let () =
	  Format.fprintf f "Rules self influence@]@,</label>@]@,</form>@," in
	let () = Format.fprintf
		   f "@[<v 2><script>@,\"use strict\"@,@[var flux =@ %a;@]@,"
		   print_json_of_flux flux in
	let () =
	  Format.fprintf
	    f
	    "var selectedRules=flux.rules.map(function () {return true;}),@," in
	let () = Format.fprintf f "selfInfluence = false;@," in
	let () =
	  Format.fprintf
	    f
	    "function filterRules (val,id) { return selectedRules[id]; }@," in
	let () =
	  Format.fprintf
	    f
	    "function pointValue (i,j,e) {@," in
	let () =
	  Format.fprintf
	    f
	    "var correction = document.getElementById(\"correction\").value;@," in
	let () = Format.fprintf f "if (selfInfluence || i !== j)@,{@," in
	let () = Format.fprintf f "if (correction === \"hits\")@," in
	let () =
	  Format.fprintf
	    f
	    "{return (flux.hits[i] === 0.) ? 0 : Math.abs(e) / flux.hits[i];}@," in
	let () = Format.fprintf f "else if (correction === \"time\")@," in
	let () =
	  Format.fprintf
	    f
	    "{return Math.abs(e) / (flux.bio_end_time - flux.bio_begin_time);}@," in
	let () =
	  Format.fprintf
	    f
	    "else {return Math.abs(e);}@,}@,else {return 0;}@,}@,@," in
	let () = Format.fprintf f "@[<v 2>function drawDIM () {@," in
	let () =
	  Format.fprintf
	    f
	    "var @[matrix = @[flux@,.fluxs@,.map(@[function(a,i)" in
	let () =
	  Format.fprintf
	    f
	    "{return a.map(function (e,j)@ {return pointValue (i,j,e)}@])" in
	let () = Format.fprintf
		   f "@,.filter(filterRules);})@,.filter(filterRules),@]@ " in
	let () =
	  Format.fprintf
	    f
	    "rules = flux.rules.filter(filterRules),@ " in
	let () =
	  Format.fprintf
	    f
	    "color = @[flux.fluxs.map(function(a)@ " in
	let () =
	  Format.fprintf
	    f
	    "{return a.map(function (x) {return (x < 0) ? \"#FF0000\" : \"#00FF00\";})@," in
	let () =
	  Format.fprintf
	    f
            ".filter(filterRules);}).filter(filterRules)@];@]@," in

	let () =
	  Format.fprintf
	    f "var chord = @[d3.@,layout.@,chord()@,.padding(.01)" in
	let () =
	  Format.fprintf
	    f "@,.sortSubgroups(d3.descending)@,.matrix(matrix);@]@," in
	let () =
	  Format.fprintf
	    f "@[var width = 960,@ height = 700,@ " in
	let () =
	  Format.fprintf
	    f "innerRadius = Math.min(width, height) * .37;@]@," in
	let () =
	  Format.fprintf
	    f "var arc = @[d3@,.svg@,.arc()@,.innerRadius(innerRadius)" in
	let () =
	  Format.fprintf
	    f "@,.outerRadius(innerRadius + 8);@]@," in
	let () =
	  Format.fprintf
	    f "var svg = @[d3@,.select(\"body\")@,.select(\"svg\")" in
	let () =
	  Format.fprintf
	    f "@,.attr(\"width\", width)@,.attr(\"height\", height)" in
	let () =
	  Format.fprintf
	    f "@,.select(\"g\").attr(\"transform\", \"translate(\" + width / 2 + \",\" + height / 2 + \")\");@]@," in
	let () = Format.fprintf f "svg.selectAll(\"*\").remove();@," in
	let () =
	  Format.fprintf
	    f "@[svg.append(\"g\")@,.attr(\"class\", \"chord\")" in
	let () =
	  Format.fprintf
	    f "@,.selectAll(\"path\")@,.data(chord.chords)@,.enter()" in
	let () =
	  Format.fprintf
	    f "@,.append(\"path\")@,.attr(\"d\", d3.svg.chord().radius(innerRadius))" in
	let () =
	  Format.fprintf
	    f "@,.style(\"fill\", function(d) { return color[d.source.index][d.target.index]; })@,.style(\"opacity\", 1);@]@," in

	let () =
	  Format.fprintf
	    f "svg.append(\"g\").attr(\"id\", \"values\").selectAll(\".sources\")@," in
	let () =
	  Format.fprintf
	    f ".data(chord.chords).enter().append(\"text\").attr(\"class\",\"sources\")@," in
	let () =
	  Format.fprintf
	    f ".each(function(d) { d.angle = ( d.source.startAngle + d.source.endAngle) / 2; })@," in
	let () =
	  Format.fprintf
	    f ".attr(\"dy\", \".1em\")@," in
	let () =
	  Format.fprintf
	    f ".attr(\"transform\", function(d) {@," in
        let () =
	  Format.fprintf
	    f "return \"rotate(\" + (d.angle * 180 / Math.PI - 90) + \")\"@," in
        let () =
	  Format.fprintf
	    f "+ \"translate(\" + (innerRadius - 10) + \")\"@," in
        let () =
	  Format.fprintf
	    f "+ (d.angle > Math.PI ? \"rotate(180)\" : \"\"); })@," in
	let () =
	  Format.fprintf
	    f ".style(\"text-anchor\", function(d) { return d.angle > Math.PI ? null : \"end\" ; })@," in
	let () =
	  Format.fprintf
	    f ".text(function (d) { return d.source.value;});@," in
	let () =
	  Format.fprintf
	    f "svg.select(\"#values\").selectAll(\".targets\")@," in
	let () =
	  Format.fprintf
	    f ".data(chord.chords).enter()@," in
	let () =
	  Format.fprintf
	    f ".append(\"text\").attr(\"class\",\"targets\")@," in
	let () =
	  Format.fprintf
	    f ".each(function(d) { d.angle = ( d.target.startAngle + d.target.endAngle) / 2; })@," in
	let () =
	  Format.fprintf
	    f ".attr(\"dy\", \".1em\")@," in
	let () =
	  Format.fprintf
	    f ".attr(\"transform\", function(d) {@," in
        let () =
	  Format.fprintf
	    f "return \"rotate(\" + (d.angle * 180 / Math.PI - 90) + \")\"@," in
        let () =
	  Format.fprintf
	    f "+ \"translate(\" + (innerRadius - 10) + \")\"@," in
        let () =
	  Format.fprintf
	    f "+ (d.angle > Math.PI ? \"rotate(180)\" : \"\"); })@," in
	let () =
	  Format.fprintf
	    f ".style(\"text-anchor\", function(d) { return d.angle > Math.PI ? null : \"end\" ; })@," in
	let () =
	  Format.fprintf
	    f ".text(function (d) { return d.target.value;});@," in

	let () =
	  Format.fprintf
	    f "var legends = @[svg@,.append(\"g\")@,.selectAll(\"g\")@,.data(chord.groups)" in
	let () =
	  Format.fprintf
	    f "@,.enter()@,.append(\"g\");@]@," in
	  let () =
	  Format.fprintf
	    f "@[legends@,.append(\"text\")@,.each(function(d) { d.angle = (d.startAngle + d.endAngle) / 2; })" in
	let () =
	  Format.fprintf
	    f "@,.attr(\"dy\", \".1em\")@,.attr(\"transform\",@[ function(d) {@ " in
	let () =
	  Format.fprintf
	    f "return \"rotate(\" + (d.angle * 180 / Math.PI - 90) + \")\"@ " in
	let () =
	  Format.fprintf
	    f "+ \"translate(\" + (innerRadius + 10) + \")\"@ + (d.angle > Math.PI ? \"rotate(180)\" : \"\");@ }@])" in
	let () =
	  Format.fprintf
	    f "@,.style(\"text-anchor\", function(d) { return d.angle > Math.PI ? \"end\" : null; })" in
	let () =
	  Format.fprintf
	    f "@,.text(function(d) { return rules[d.index]; });@]@," in
	let () =
	  Format.fprintf
	    f "legends@[@,.append(\"path\")@,.style(\"fill\", \"#222222\")"in
	let () =
	  Format.fprintf
	    f "@,.attr(\"d\", arc)@,.on(\"mouseover\", fade(svg,.1))@,.on(\"mouseout\", fade(svg,1));@]@]@,}@," in
	let () =
	  Format.fprintf
	    f "// Returns an event handler for fading a given chord group.@," in
	let () =
	  Format.fprintf
	    f "@[function fade(svg,opacity) {@ return function(g, i) {@ " in
	let () = Format.fprintf f "svg@,.selectAll(\".chord path\")@,." in
	let () =
	  Format.fprintf
	    f "filter(function(d) { return d.source.index != i && d.target.index != i; })" in
	let () =
	  Format.fprintf
	    f "@,.transition()@,.style(\"opacity\", opacity);@ };@ }@]@,@," in

	let () =
	  Format.fprintf
	    f "@[<v 2>function aClick (id) {@," in
	let () =
	  Format.fprintf
	    f "selectedRules[id] = (selectedRules[id]) ? false : true;@," in
	let () =
	  Format.fprintf
	    f "drawDIM();@]@,}@," in
	let () =
	  Format.fprintf
	    f "@[<v 2>function toggleSelfInfluence () {@," in
	let () =
	  Format.fprintf
	    f "selfInfluence = (selfInfluence) ? false : true;@," in
	let () =
	  Format.fprintf
	    f "drawDIM();@]@,}@,@," in

	let () =
	  Format.fprintf
	    f "@[<v 2>function populate() {@," in
	let () =
	  Format.fprintf
	    f "var menu = document.getElementById(\"menu\");@," in
	let () =
	  Format.fprintf
	    f "@[<v 2>selectedRules.forEach(function (val,id,a) {@," in
	let () =
	  Format.fprintf
	    f "var boxbox = document.createElement(\"label\"),@," in
	let () =
	  Format.fprintf
	    f "box = document.createElement(\"input\");@," in
	let () =
	  Format.fprintf
	    f "boxbox.setAttribute(\"class\",\"checkbox-inline\")@," in
	let () =
	  Format.fprintf
	    f "box.setAttribute(\"type\", \"checkbox\");@," in
	let () =
	  Format.fprintf
	    f "box.setAttribute(\"checked\", val);@," in
	let () =
	  Format.fprintf
	    f "box.addEventListener(\"change\",function () { aClick(id);});@," in
	let () =
	  Format.fprintf
	    f "boxbox.appendChild(box);@," in
	let () =
	  Format.fprintf
	    f "boxbox.appendChild(document.createTextNode(flux.rules[id]));@," in
	let () =
	  Format.fprintf
	    f "menu.appendChild(boxbox)@]@,});@," in
	let () =
	  Format.fprintf
	    f "drawDIM();@]@,}@," in
let () =
	  Format.fprintf
	    f "populate();" in

	Format.fprintf f "@]@,</script>"))

let output_flux env counter b =
  let out = stop_flux env counter b in
  if Filename.check_suffix out.flux_data.flux_name ".html"
  then html_of_flux out
  else if Filename.check_suffix out.flux_data.flux_name ".json"
  then json_of_flux out
  else dot_of_flux out
