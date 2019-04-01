#! /bin/sh

make

rm -r KaSa_rep/logic_encoding/demo/output/

for TEST in "001" "002"
do
    echo "############################################################"
    echo "TEST $TEST"
    echo "############################################################"

    mkdir -p KaSa_rep/logic_encoding/demo/output/$TEST
    bin/KaSa KaSa_rep/logic_encoding/demo/$TEST.ka \
        --output-directory KaSa_rep/logic_encoding/demo/output/$TEST \
    #    --debug \
    #    --verbosity-level-for-reachability-analysis Full \
    #    --output-mode-for-reachability-analysis raw \
        > KaSa_rep/logic_encoding/demo/output/$TEST/LOG.debug

    cat KaSa_rep/logic_encoding/demo/output/$TEST/LOG.debug
done

# Here is the list of recognized options

# General options
#   --help            Verbose help
#    -h               Short help
#   --version         Show version number
#   --gui             GUI to select
#   --(no-)expert     Expert mode (more options)
# Actions
#   --do-all
#   --reset-all
#   --(no-)compute-contact-map    (default: enabled)
#   --(no-)compute-influence-map    (default: enabled)
#   --(no-)compute-potential-cycles    (default: disabled)
#   --(no-)compute-reachability-analysis    (default: enabled)
#   --(no-)compute-symmetries    (default: disabled)
#   --(no-)compute-local-traces    (default: disabled)
#   --(no-)compute-separating-transitions    (default: disabled)
#   -syntax V3 | V4 
#  (default: V4)
# Syntax
#   -syntax V3 | V4 
#  (default: V4)
# Output
#   --output-directory <value>
#   --output-contact-map-directory <name>   (default: output)
#   --output-contact-map <name>   (default: contact)
#   --output-influence-map-directory <name>   (default: output)
#   --output-influence-map <name>   (default: influence)
#   --influence-map-format DOT | DIM | HTML 
#  (default: DOT)
#   --output-local-traces-directory <name>   (default: output)
#   --local-traces-format DOT | HTML 
#  (default: DOT)
# Reachability analysis
#   --(no-)compute-reachability-analysis    (default: enabled)
#   --enable-every-domain
#   --disable-every-domain
#   --contact-map-domain static | dynamic 
#  (default: dynamic)
#   --(no-)views-domain    (default: enabled)
#   --(no-)counters-domain    (default: enabled)
#   --counters-accuracy non-relational | octagons | mi 
#  (default: mi)
#   --(no-)double-bonds-domain    (default: enabled)
#   --(no-)sites-across-bonds-domain    (default: enabled)
#   --verbosity-level-for-reachability-analysis Mute | Low | Medium | High |
#                                               Full 
#  (default: Low)
#   --output-mode-for-reachability-analysis raw | kappa | english 
#  (default: kappa)
# Trace analysis
#   --(no-)compute-local-traces    (default: disabled)
#   --(no-)show-rule-names-in-local-traces    (default: enabled)
#   --(no-)use-macrotransitions-in-local-traces    (default: disabled)
#   --(no-)compute-separating-transitions    (default: disabled)
#   --output-local-traces-directory <name>   (default: output)
#   --local-traces-format DOT | HTML 
#  (default: DOT)
# Contact map
#   --(no-)compute-contact-map    (default: enabled)
#   --(no-)compute-potential-cycles    (default: disabled)
#   --output-contact-map-directory <name>   (default: output)
#   --output-contact-map <name>   (default: contact)
#   --contact-map-accuracy-level Low | High 
#  (default: Low)
#   --polymer-detection-accuracy-level Low | High 
#  (default: High)
# Influence map
#   --(no-)compute-influence-map    (default: enabled)
#   --influence-map-accuracy-level Indirect | Direct | Realisable 
#  (default: Direct)
#   --output-influence-map-directory <name>   (default: output)
#   --output-influence-map <name>   (default: influence)
#   --influence-map-format DOT | DIM | HTML 
#  (default: DOT)
# (53 options)