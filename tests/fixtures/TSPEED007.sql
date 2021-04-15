-- Public Domain
-- Taken from https://www.nlm.nih.gov/research/umls/rxnorm/sourcereleasedocs/rxnorm.html
-- TSPEED007 - Speed Test RxNorm Sample 2
SELECT rc1.rxcui, rc1.str as rxnorm_str, rr.rela, rc2.tty as rxnorm_tty, rc2.str as rxnorm_bn, rc2.suppress
FROM rxnconso rc1, rxnrel rr, rxnconso rc2
WHERE rc1.rxcui = '5640'
     AND rc1.sab = 'RXNORM'
     AND rc1.rxcui = rr.rxcui2
     AND rr.rela = 'has_tradename'
     AND rr.rxcui1 = rc2.rxcui
     AND rc2.sab = 'RXNORM'
     AND rc2.tty = 'BN'
     ORDER BY rc2.suppress;
