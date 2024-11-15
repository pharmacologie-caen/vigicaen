# basic use and here package works

    Code
      tb_main(path_base = path_base, path_sub = path_sub)
    Output
      Read DEMO.txt                 3%Split demo                    6%Write demo.parquet           12%Read DRUG.txt                16%Split drug                   20%Write drug.parquet           27%Read FOLLOWUP.txt            30%Split followup               32%Write followup.parquet       34%Read ADR.txt                 36%Split adr                    41%Write adr.parquet            48%Read OUT.txt                 51%Split out                    53%Write out.parquet            55%Read SRCE.txt                57%Split srce                   59%Write srce.parquet           61%Read LINK.txt                63%Split link                   68%Write link.parquet           75%Read IND.txt                 78%Split ind                    88%Write ind.parquet            92%Read SUSPECTEDDUPLICATES.txt 95%Split suspdup                97%Write suspdup.parquet        99%Done                             

---

    Code
      tb_main(path_base = here_path_base, path_sub = here_path_sub)
    Output
      Read DEMO.txt                 3%Split demo                    6%Write demo.parquet           12%Read DRUG.txt                16%Split drug                   20%Write drug.parquet           27%Read FOLLOWUP.txt            30%Split followup               32%Write followup.parquet       34%Read ADR.txt                 36%Split adr                    41%Write adr.parquet            48%Read OUT.txt                 51%Split out                    53%Write out.parquet            55%Read SRCE.txt                57%Split srce                   59%Write srce.parquet           61%Read LINK.txt                63%Split link                   68%Write link.parquet           75%Read IND.txt                 78%Split ind                    88%Write ind.parquet            92%Read SUSPECTEDDUPLICATES.txt 95%Split suspdup                97%Write suspdup.parquet        99%Done                             

---

    Code
      tb_main(path_base = path_base, path_sub = here_path_sub)
    Output
      Read DEMO.txt                 3%Split demo                    6%Write demo.parquet           12%Read DRUG.txt                16%Split drug                   20%Write drug.parquet           27%Read FOLLOWUP.txt            30%Split followup               32%Write followup.parquet       34%Read ADR.txt                 36%Split adr                    41%Write adr.parquet            48%Read OUT.txt                 51%Split out                    53%Write out.parquet            55%Read SRCE.txt                57%Split srce                   59%Write srce.parquet           61%Read LINK.txt                63%Split link                   68%Write link.parquet           75%Read IND.txt                 78%Split ind                    88%Write ind.parquet            92%Read SUSPECTEDDUPLICATES.txt 95%Split suspdup                97%Write suspdup.parquet        99%Done                             

---

    Code
      tb_main(path_base = here_path_base, path_sub = path_sub)
    Output
      Read DEMO.txt                 3%Split demo                    6%Write demo.parquet           12%Read DRUG.txt                16%Split drug                   20%Write drug.parquet           27%Read FOLLOWUP.txt            30%Split followup               32%Write followup.parquet       34%Read ADR.txt                 36%Split adr                    41%Write adr.parquet            48%Read OUT.txt                 51%Split out                    53%Write out.parquet            55%Read SRCE.txt                57%Split srce                   59%Write srce.parquet           61%Read LINK.txt                63%Split link                   68%Write link.parquet           75%Read IND.txt                 78%Split ind                    88%Write ind.parquet            92%Read SUSPECTEDDUPLICATES.txt 95%Split suspdup                97%Write suspdup.parquet        99%Done                             

