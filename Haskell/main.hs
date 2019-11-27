-- Imports

import Parser
import DataTypes
import Data.Time.Clock
import Data.Time.Calendar
 
-- Datas
getYear (GregorianCalendar ano _ _ ) = ano
getMonth (GregorianCalendar _ mes _ ) = mes
getDay (GregorianCalendar _ _ dia) = dia

-- Auxiliares
anoSelecionado ano (Transacao calendario _ _ _ _ _) = ano == getYear calendario
anoMesSelecionado ano mes (Transacao calendario _ _ _ _ _) = ano == getYear calendario && mes == getMonth calendario
anoMesDiaSelecionado ano mes dia (Transacao calendario _ _ _ _ _) = ano == getYear calendario && mes == getMonth calendario && dia <= getDay calendario

getValor (Transacao _ valor _ _ _ _) = valor
getData (Transacao datas _ _ _ _ _) = datas
getTipo (Transacao _ _ _ _ _ tipo) = tipo

getValorTipo (Transacao _ valor _ _ _ tipo) = (valor, tipo)

somaValoresTransacao [] = 0
somaValoresTransacao (x:xs) = getValor x + somaValoresTransacao xs 

receita (Transacao _ valor _ _ _ tipo) = (not (SALDO_CORRENTE `elem` tipo)) && (not (VALOR_APLICACAO `elem` tipo)) && (not (APLICACAO `elem` tipo))  && (valor > 0)
despesa (Transacao _ valor _ _ _ tipo) = (not (SALDO_CORRENTE `elem` tipo)) && (not (VALOR_APLICACAO `elem` tipo)) && (not (APLICACAO `elem` tipo)) && (valor < 0)
receitaOuDespesa (Transacao _ valor _ _ _ tipo) = (not (SALDO_CORRENTE `elem` tipo)) && (not (VALOR_APLICACAO `elem` tipo)) && (not (APLICACAO `elem` tipo))
todasAsTransacoes (Transacao _ valor _ _ _ tipo) = (not (VALOR_APLICACAO `elem` tipo)) && (not (APLICACAO `elem` tipo)) 

calculaSaldos' [] _ = []
calculaSaldos' (x:xs) b = [b] ++ calculaSaldos' xs ((getValor x)+b)

--Funcionalidades

--Filtrar transacoes 
filtrarTransacoes arquivo = filter (todasAsTransacoes) arquivo

-- Filtrar transações por ano.
filtraTransacoesPorAno arquivo ano = filter (anoSelecionado ano) arquivo

--Filtrar transações por ano e mês.
filtraPorAnoEMes arquivo ano mes = filter (anoMesSelecionado ano mes) arquivo

--Filtra receita por ano e mes.
filtraReceitasPorAnoMes arquivo ano mes = somaValoresTransacao (filter (receita) (filtraPorAnoEMes arquivo ano mes))

--Filtra receita por ano mes e dia.
filtraPorAnoMesDia arquivo ano mes dia = filter (anoMesDiaSelecionado ano mes dia) arquivo 
filtraReceitasPorAnoMesDia arquivo ano mes dia = somaValoresTransacao (filter (receita) (filtraPorAnoMesDia arquivo ano mes dia))

--Filtra despesa por ano e mes.
filtraDespesasPorAnoMes arquivo ano mes = somaValoresTransacao (filter (despesa) (filtraPorAnoEMes arquivo ano mes))

--Filtra despesa por ano mes e dia.
filtraDespesasPorAnoMesDia arquivo ano mes dia = somaValoresTransacao (filter (despesa) (filtraPorAnoMesDia arquivo ano mes dia))

--Calcula sobras por ano e mes
calculaSobrasAnoMes arquivo ano mes = (filtraReceitasPorAnoMes arquivo ano mes) + (filtraDespesasPorAnoMes arquivo ano mes) 

--Calcula sobras por ano, mes e dia
calculaSobrasAnoMesDia arquivo ano mes dia = (filtraReceitasPorAnoMesDia arquivo ano mes dia) + (filtraDespesasPorAnoMesDia arquivo ano mes dia)

--Calcula saldo por ano e mes
calculaSaldos arquivo ano mes = (calculaSaldos' (filter (receitaOuDespesa) (filtraPorAnoEMes arquivo ano mes)) (getValor (head (filter (todasAsTransacoes) ((filtraPorAnoEMes arquivo ano mes)))) ))

--Calcula saldo maximo no ano e mes
calculaSaldoMaximo [] _ _ = 0
calculaSaldoMaximo arquivo ano mes = maximum (calculaSaldos arquivo ano mes)

--Calcula saldo minimo no ano e mes
calculaSaldoMinimo [] _ _ = 0
calculaSaldoMinimo arquivo ano mes = minimum (calculaSaldos arquivo ano mes)
 
--Calcula media de receitas por ano
calculaMediaReceitasAno arquivo ano = somaValoresTransacao (filter (receita) (filtraTransacoesPorAno arquivo ano)) / ( fromIntegral (length (filter (receita) (filtraTransacoesPorAno arquivo ano))))

--Calcula media de despesas por ano
calculaMediaDespesasAno arquivo ano = somaValoresTransacao (filter (despesa) (filtraTransacoesPorAno arquivo ano)) / ( fromIntegral (length (filter (despesa) (filtraTransacoesPorAno arquivo ano))))

--Calcula media de sobras por ano

calculaSobra [] = 0
calculaSobra (x:xs) = x + calculaSobra xs

calculaMediaSobrasAno arquivo ano = (calculaSobra (map (calculaSobrasAnoMes arquivo ano) [1..12]))/12