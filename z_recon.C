# include < string >
# include < iostream >
# include < TF1 .h >
# include < TFile .h >
# include < TTree .h >
# include < TMath .h >
# include < TROOT .h >
# include < TRandom3 .h >
# include < TTreeFormula.h>



Double_t v_z (Double_t *zval, Double_t *p)
{
  // Velocity as a function of z-in-the-target assuming constant density
  // Parameters [0] Stopping power in MeV/cm
  //            [1] Beam energy in MeV
  //            [2] beam mass in amu
  const Double_t c = 29.9792458; // cm/ns
  const Double_t amu = 931.494; // MeV/c**2
  //return c * TMath::Sqrt( (2.*p[1]/(p[2]*amu)) - (2.*p[0]*(z+6.15)/(p[2]*amu)) );
 
  Double_t z = *zval;
  return c * TMath::Sqrt( (2.*p[1]/(p[2]*amu)) - (2.*p[0]*(z+6.15)/(p[2]*amu)) );
}

// Double_t v_inv (Double_t *zval, Double_t *p)
// {
//    // Velocity as a function of z-in-the-target assuming constant density
//   // Parameters [0] Stopping power in MeV/cm
//   //            [1] Beam energy in MeV
//   //            [2] beam mass in amu
//   const Double_t c = 29.9792458; // cm/ns
//   const Double_t amu = 931.494; // MeV/c**2
//   //return c * TMath::Sqrt( (2.*p[1]/(p[2]*amu)) - (2.*p[0]*(z+6.15)/(p[2]*amu)) );
 
//   Double_t z = *zval;
//   return 1. /(c * TMath::Sqrt( (2.*p[1]/(p[2]*amu)) - (2.*p[0]*(z+6.15)/(p[2]*amu)) )); 
// }


 


void z_recon()
{
  const Double_t c = 29.9792458; // cm/ns
  const Double_t amu = 931.494; // MeV/c**2
  Double_t Ebeam = 11.593; // MeV
  Double_t Stopping = 0.0295; // MeV/cm
  Double_t Mass = 23.; // amu
  Double_t Vbeam = c*TMath::Sqrt(2.*Ebeam/(Mass*amu));
  Double_t Vout = c*TMath::Sqrt(2.*(Ebeam-Stopping*12.3)/(Mass*amu));
  
  TF1 *fV_z = new TF1("fV_z",v_z,-6.15,6.15,3);
  fV_z->SetParameter(0,Stopping); // MeV/cm
  fV_z->SetParameter(1,Ebeam); // MeV
  fV_z->SetParameter(2,Mass); // amu
  
 
  

  



  //fV_z->Draw();
  // Assume V(z) is linear and find equation:
  Double_t grad = (fV_z->Eval(6.15)-fV_z->Eval(-6.15))/(12.3);
  cout << grad << endl;
  Double_t intercept = fV_z->Eval(0);
  cout << intercept << endl;

  // Create the inverse velocity function 1/V(t)
  TF1 *fV_inv = new TF1("fV_inv","1/([0]*x+[1])",-6.15,6.15);
  fV_inv->SetParameter(0,grad);
  fV_inv->SetParameter(1,intercept);
  //fV_inv->Draw();

  // Create a graph of the integrals of 1/V(z), which is equal to time passed
  const int nstep = 1000;
  Double_t x[nstep], y[nstep];
  for (int i=0; i<nstep; i++){
    double z = -6.15 + double(i)*12.3/double(nstep);
    //cout << z << endl;
    Double_t f_z = fV_inv->Integral(-6.15,z);
    x[i] = z;
    y[i] = f_z;
  }
  TGraph *g1 = new TGraph(nstep,x,y);
  //g1->Draw();

  // Fit Straight line to graph, again assuming linear approximation, get fit parameters
  g1->Fit("pol1");
  TF1 *myfunc=g1->GetFunction("pol1");
  Double_t p0=myfunc->GetParameter(0);
  Double_t p1=myfunc->GetParameter(1);

  





  // Variables for correction

  Double_t Dist_scint = -87.912;
  Double_t t0 = (TMath::Abs(Dist_scint)-6.15)/(c*TMath::Sqrt(2.*Ebeam/(Mass*amu)));
  cout << t0 << endl;

  TH1F *hz0 = new TH1F("hz0","",100,-15,15);
  TH1F *hz1 = new TH1F("hz1","",100,-15,15);
  TH1F *hz2 = new TH1F("hz2","",100,-15,15);
  TH1F *hz3 = new TH1F("hz3","",100,-15,15);
  TH1F *hz4 = new TH1F("hz4","",100,-15,15);
  TH1F *hz5 = new TH1F("hz5","",100,-15,15);

  TTreeFormula *f1 = new TTreeFormula("f1","((gammatof-[0])-[2])/[1]",h1001);
  f1->SetParameter(0,t0);
  f1->SetParameter(1,p0);
  f1->SetParameter(2,p1);
  
 
//h1001->Draw("(gammatof*29.9792458*TMath::Sqrt(2.*10.078/(26.*931.494))+Dist_scint)>>hz","gammatof!=0");
  //h1001->Draw("gammatof","gammatof!=0");
  //h1001->Draw(f1,"gammatof!=0");
  //cout << f1->Eval(102) << endl;


  cout << t0 << "\t" << p0 << "\t" << p1 << endl;
  
  h1001->Draw("((gammatof-94.5378)*0.864-6.15)>>hz0","gammatof!=0","",3,0);
  h1001->Draw("((gammatof-94.5378)*0.864-6.15)>>hz1","gammatof!=0","",10,0);
  h1001->Draw("((gammatof-94.5378)*0.864-6.15)>>hz2","gammatof!=0","",20,0);
  h1001->Draw("((gammatof-94.5378)*0.864-6.15)>>hz3","gammatof!=0","",50,0);
  h1001->Draw("((gammatof-94.5378)*0.864-6.15)>>hz4","gammatof!=0","",100,0);
  
  // h1001->Draw("(gammatof-94.5)*(0.864)-87.912","gammatof!=0");

  Double_t mean[5];
  Double_t sig[5];
  Double_t cts[5];
  
  mean[0] = hz0->GetMean();
  //cout << hz0->Integral() << endl;
  sig[0] = hz0->GetRMS()/TMath::Sqrt(hz0->Integral());
  cts[0] = hz0->Integral();
  mean[1] = hz1->GetMean(); 
  sig[1] = hz1->GetRMS()/TMath::Sqrt(hz1->Integral());
  cts[1] = hz1->Integral(); 
  mean[2] = hz2->GetMean();
  sig[2] = hz2->GetRMS()/TMath::Sqrt(hz2->Integral());
  cts[2] = hz2->Integral();
  mean[3] = hz3->GetMean();
  sig[3] = hz3->GetRMS()/TMath::Sqrt(hz3->Integral());
  cts[3] = hz3->Integral();
  mean[4] = hz4->GetMean();
  sig[4] = hz4->GetRMS()/TMath::Sqrt(hz4->Integral());
  cts[4] = hz4->Integral();

  hz0->GetXaxis()->SetTitle("Reconstructed z_{0} position (cm");
  hz0->GetXaxis()->CenterTitle();
  hz0->GetYaxis()->SetTitle("Counts");
  hz0->GetYaxis()->CenterTitle();
  hz0->SetTitle("Reconstructed z_{0} position from BGO hit-pattern");
  hz1->GetXaxis()->SetTitle("Reconstructed z_{0} position (cm");
  hz1->GetXaxis()->CenterTitle();
  hz1->GetYaxis()->SetTitle("Counts");
  hz1->GetYaxis()->CenterTitle();
  hz1->SetTitle("Reconstructed z_{0} position from BGO hit-pattern");
  hz2->GetXaxis()->SetTitle("Reconstructed z_{0} position (cm");
  hz2->GetXaxis()->CenterTitle();
  hz2->GetYaxis()->SetTitle("Counts");
  hz2->GetYaxis()->CenterTitle();
  hz2->SetTitle("Reconstructed z_{0} position from BGO hit-pattern");
  hz3->GetXaxis()->SetTitle("Reconstructed z_{0} position (cm");
  hz3->GetXaxis()->CenterTitle();
  hz3->GetYaxis()->SetTitle("Counts");
  hz3->GetYaxis()->CenterTitle();
  hz3->SetTitle("Reconstructed z_{0} position from BGO hit-pattern");
  hz4->GetXaxis()->SetTitle("Reconstructed z_{0} position (cm");
  hz4->GetXaxis()->CenterTitle();
  hz4->GetYaxis()->SetTitle("Counts");
  hz4->GetYaxis()->CenterTitle();
  hz4->SetTitle("Reconstructed z_{0} position from BGO hit-pattern");
  
  hz0->Draw();



  /*
  
  TCanvas *canv = new TCanvas("canv","canv",200,10,700,500);
  //canv->SetLogx(1);
  
  // Find true mean
  TH1F *h_true = new TH1F("h_true","",100,-25,25);
  h1000->Draw("zint>>h_true","react==1");
  Float_t z_true = h_true->GetMean();
  TF1 *f_true = new TF1("f_true","[0]*x + [1]",0,5000);
  f_true->SetParameter(0,0);
  f_true->SetParameter(1,z_true);
  f_true->SetLineColor(2);
  f_true->SetLineStyle(2);
  f_true->SetLineWidth(2);
  

  TGraphErrors *g2 = new TGraphErrors(5,cts,mean,0,sig);
  g2->GetYaxis()->SetRangeUser(-5,5);

  g2->GetXaxis()->SetTitle("Number of Events in #gamma_{0} Spectrum");
  g2->GetXaxis()->CenterTitle();
  g2->GetYaxis()->SetTitle("Reconstructed z_{0} position (cm)");
  g2->GetYaxis()->CenterTitle();
  g2->SetTitle("Reconstructed z_{0} position from BGO hit-pattern");
  g2->Draw("ALP");
  f_true->Draw("Same");

  cout << "V_beam = " << "\t" << c*TMath::Sqrt(2.*Ebeam/(Mass*amu)) << " cm/ns" << endl;
  cout << "V_out = " << "\t" << Vout << " cm/ns" << endl;

  */
  
}
/*
double func() {
  double x;
  return x-
}
*/

