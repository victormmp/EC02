data_2016<-read.csv("imc_20162.csv");

data_2017<-read.csv("CS01_20172.csv", sep=";");

PPGEE_dados=data_2016[data_2016[2]=='PPGEE',];

Dados_Masculino_2016=PPGEE_dados[PPGEE_dados[3]=='M',];
Dados_Feminino_2016=PPGEE_dados[PPGEE_dados[3]=='F',];

Heigh_Maculino_2016=Dados_Masculino_2016[,4];
Heigh_Feminino_2016=Dados_Feminino_2016[,4];
Weight_Masculino_2016=Dados_Masculino_2016[,5];
Weight_Feminino_2016=Dados_Feminino_2016[,5];
#Calculo do IMC para popula????o feminina e masculina do ano de 2016.
IMC_masculino_2016=(Weight_Masculino_2016/((Heigh_Maculino_2016)*(Heigh_Maculino_2016)));
IMC_Feminino_2016=(Weight_Feminino_2016/((Heigh_Feminino_2016)*(Heigh_Feminino_2016)));
####################################################################################
Dados_Masculino_2017=data_2017[data_2017[3]=='M',];
Dados_Feminino_2017=data_2017[data_2017[3]=='F',];

Heigh_Maculino_2017=Dados_Masculino_2017[,2];
Heigh_Feminino_2017=Dados_Feminino_2017[,2];
Weight_Masculino_2017=Dados_Masculino_2017[,1];
Weight_Feminino_2017=Dados_Feminino_2017[,1];

#Calculo do IMC para popula????o feminina e masculina do ano de 2017.
IMC_masculino_2017=(Weight_Masculino_2017/((Heigh_Maculino_2017)*(Heigh_Maculino_2017)));
IMC_Feminino_2017=(Weight_Feminino_2017/((Heigh_Feminino_2017)*(Heigh_Feminino_2017)));

#Teste para verificar normalidade em popula????es femininas e masculinas de cada ano.
teste_normalidade_2016_feminino=shapiro.test(as.numeric(unlist(IMC_Feminino_2016)));
teste_normalidade_2017_feminino=shapiro.test(as.numeric(unlist(IMC_Feminino_2017)));

teste_normalidade_2016_masculino=shapiro.test(as.numeric(unlist(IMC_masculino_2016)));
teste_normalidade_2017_masculino=shapiro.test(as.numeric(unlist(IMC_masculino_2017)));

#Realizando o teste T para comparacao das medias.
#Teste entre populacoes masculinas de 2016 e 2017.

t.test(as.numeric(unlist(IMC_masculino_2016)),as.numeric(unlist(IMC_masculino_2017)), alternative='two.sided',mu=0, paired=FALSE,conf.level = 0.95);

#test que compara as medianas dos dois grupos femininos de 2016 e 2017.
wilcox.test(as.numeric(unlist(IMC_Feminino_2016)),as.numeric(unlist(IMC_Feminino_2017)))

