cat kasa_options.head > ../generated_img/KaSa_options.txt &&\
"${KAPPABIN}"KaSa --help --expert  >> ../generated_img/KaSa_options.txt &&\
cat kasa_options.foot >> ../generated_img/KaSa_options.txt
