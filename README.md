# API_INMET
Busca dados climáticos usando a API do INMET

Script para buscar de dados climáticos do INMET, através de API (https://portal.inmet.gov.br/manual/manual-de-uso-da-api-esta%C3%A7%C3%B5es).
A primeira parte do script retorna uma tabela com as estações automáticas e manuais do INMET. São retornados dados como o código único da estação (cada uma tem um), Estado e coordenadas em que ela está e data de início de operação.

Após a tabela mencionada ser gerada, os códigos das estações presentes em um vetor de Estados de exemplo são filtrados e as informações meteorológicas dos mesmos são obtidas e passadas para uma tabela. São pegos os dados **diários** e como algumas estações operam há vários anos, o vetor de Estados do exemplo cria uma tabela grande, que demora vários minutos para ser formada.
As colunas presentes na tabela de Clima são:
1. Temperatura máxima diária (°C)
2. Umidade média diária (%)
3. UF
4. Data
5. Nome da estação
6. Precipitação diária (mm)
7. Código da estação
8. Latitude
9. Longitude
10. Temperatura mínima (°C)
11. Temperatura média (°C)

Dois gráficos são criados em seguida, com a tabela para os Estados, como exemplo.
Um com as temperaturas médias anuais (de todas as estações) por ano e outro com as temperaturas por estação do ano, por estação meteorológica, por Estado, com linhas horizontais para marcar a média da estação do ano para todos os Estados.

Por volta de maio/2022, descobri que o pessoal do site tinha limitado o período de busca a no máximo 6 meses. Atualizei o script. Agora ele quebra o período em 6 meses e faz vários pedidos ao site automaticamente. Demora bem mais tempo, mas continua pegando os dados automaticamente.
