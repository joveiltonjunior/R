
#os Gráficos mais utilizados na petroleomica '''

library(tidyverse)
library(readxl)
library(ggplot2)

# importando os dados
df <- read_xlsx('C:\\Users\\jovei\\OneDrive\\Área de Trabalho\\Poço 1\\ESI\\Pw1.xlsx')

################################ Gráfico de distribuição de massas #################################
''' O gráfico mais simples é o de distribuição de massas ou espectro, para fazê-lo basta ordenar as 
abundâncias da menor para a maior e depois disso é só plotar o gráfico, onde no eixo x vai ser
colocado o m/z e no eixo y as intensidades'''
ggplot(data = df, aes(x = `Exp m/z`, y = `Mono Abund`)) +
  geom_line() +
  labs(
    x = "m/z",
    y = "Mono Abundância",
    title = "Gráfico de Distribuição de Classes "
  ) +
  theme_minimal()+ 
  scale_y_continuous(expand = (expansion(add = c(0,50000000))))''' essa linha de codigo e para evitar
que o gráfico fique flutuando '''

#################################### Distribuição de classes #########################################

'''Antes de fazer o gráfico, é preciso agrupar as amostras por classes e somar as mono Abundância 
de cada classe, depois criar uma coluna de porcentagem onde vão ser somadas todas as mono Abundância 
o resultado vai ser utilizado para calcular a porcentagem de abundância de cada classe '''

classes <- df %>% group_by(Class ) %>% summarise(`Mono Abund`= sum(`Mono Abund`)) %>% 
  mutate(porcentagem = `Mono Abund`/sum(`Mono Abund`)*100) %>%  arrange(desc(`Mono Abund`)) 


#Gráfico

ggplot(data = classes, aes(x= Class, y= porcentagem))+
  geom_col() + theme_light() +
  labs(x= 'Classes', y= 'Abundancia Relativa em (%)', title = 'Distribuição de Classes') +
  scale_y_continuous(expand = (expansion(add = c(0,10))))

#Distribuição das classes menores que 2%
classesminoritarias <- classes %>% filter(porcentagem < 2)

ggplot(data= classesminoritarias, aes(x= Class, y = porcentagem))+
  geom_col() + theme_light()+ 
  labs(x= 'Classes', y= 'Abundancia Relativa em (%)', title = 'Distribuição de Classes < 2%' )+
  scale_y_continuous(expand = (expansion(add = c(0,0.5))))

############################################ Distribuição de DBE#####################################

''' para fazer a distribuição de DBE é preciso filtrar somente uma classe após filtrado, é feito o 
 agrupamento dos compostos de mesmo DBE e somado as mono Abundância deles, por fim e criada uma coluna
 onde vai ser colocada a porcentagem das mono abundância de cada DBE''' 

dbe <- df %>%filter(Class == 'N') %>%  
  group_by(`Neutral DBE`) %>%  
  summarise(`Mono Abund` = sum(`Mono Abund`, na.rm = TRUE), .groups = 'drop') %>%  
  mutate(Percentual = (`Mono Abund` / sum(`Mono Abund`)) * 100)  


# Gráfico de barras 

ggplot(data= dbe, aes(x= `Neutral DBE`, y =Percentual))+ geom_col()+
  theme_light()+ 
  labs(x= 'DBE', y = 'Distribuição de DBE (%)', title = 'Distribuição de DBE da Classes N')+
  scale_y_continuous(expand = (expansion(add = c(0,5))))

#Gráfico de linhas 
ggplot(data= dbe, aes(x= `Neutral DBE`, y =Percentual))+ geom_line()+
  theme_light()+ 
  labs(x= 'DBE', y = 'Distribuição de DBE (%)', title = 'Distribuição de DBE da Classes N')+
  scale_y_continuous(expand = (expansion(add = c(0,5))))

################################### Distribuição de carbono na classe N ###############################
'''Para fazer esse gráfico, é preciso filtrar uma classe após filtrar a classe e preciso agrupar os compostos que têm o mesmo número de 
carbono e somar suas mono Abundâncias, e por fim somar as mono Abundâncias de todos os números de carbonos. Depois é só plotar o gráfico
onde no eixo x vão estar os números de carbonos e no eixo y as mono Abundâncias em porcentagem.'''

n <- df %>%
  filter(Class == 'N') %>%  group_by(`#C`) %>%  summarise(
    `Mono Abund` = sum(`Mono Abund`, na.rm = TRUE)) %>%
  mutate( porcentagem = (`Mono Abund` / sum(`Mono Abund`)) * 100)


ggplot(data=n, aes(x=`#C`, y = porcentagem))+ geom_line() +  theme_light()+
  labs (x= 'Número de Carbono', y = 'Abundancia Relativa (%)', title = 
          'Distribuição de Carbono do DBE 9 da classe N') +
  scale_y_continuous(expand = (expansion(add = c(0,0.1))))


################################ Distribuição de carbono no DBE 9 da classe N ########################
''' Para fazer a distribuição de carbonos em um DBE especifico, é preciso filtrar somente uma classe 
após filtrar, filtra-se um DBE expecifico, e somado à mono Abundância dos DBE que foram filtrados na 
classe especificada, por fim e criada uma coluna  onde vai ser colocada a porcentagem das 
mono abundância de cada DBE''' 

classen <- df %>%  
  filter(Class == 'N', `Neutral DBE` == 9) %>%   
  mutate(total_mono_abund = sum(`Mono Abund`, na.rm = TRUE), 
         porcentagem = (`Mono Abund` / total_mono_abund) * 100)


ggplot(data=classen, aes(x = `#C`, y= porcentagem)) + geom_line()+  theme_light()+
  labs (x= 'Número de Carbono', y = 'Abundancia Relativa (%)', title = 
          'Distribuição de Carbono do DBE 9 da classe N') +
  scale_y_continuous(expand = (expansion(add = c(0,1))))









