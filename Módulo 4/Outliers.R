library(rstatix)
library(dplyr)

# Adicionando dados
tabela = readxl::read_excel('dados/synadenium.xlsx')
ds <- tabela %>% select(Control, Treated)
ds <- stack(ds)

# Observando os dados com boxplot

boxplot(values~ind, data = ds)

# Procurando outliers com o método do IQR utilizando o pacote rstatix

ds %>% identify_outliers(Control)
ds %>% group_by(ind) %>%  identify_outliers()
cds <- ds %>% filter(values != 78)

boxplot(values~ind, data = cds) # revendo o boxplot após a remoção do dado


# Procurando outliers utilizando o Z-score caso os dados sejam simétricos
scale(ds$values[ds$ind == 'Control']) # essa função faz o z-score
scale(ds$values[ds$ind == 'Treated'])

# Procurando um outlier - teste de Grubb - ele é indicado para distribuicao normal. G = valor extremo - média / desvio padrao
library(outliers)
grubbs.test(ds$values[ds$ind == 'Control'])
grubbs.test(ds$values[ds$ind == 'Treated'])

# Procurando mais de um outlier - teste de rosner. -----
## A estatística usada é uma versão modificada do teste t (estatística Z modificada). Ele compara o valor extremo com a média e desvio padrão dos dados restantes (excluindo temporariamente o ponto extremo). O teste de Rosner detecta outliers de forma iterativa, ou seja, ele identifica o outlier mais extremo, remove-o temporariamente e então reavalia o restante dos dados para verificar se há outros outliers. Ele faz isso até atingir um número máximo de outliers especificado por você.


