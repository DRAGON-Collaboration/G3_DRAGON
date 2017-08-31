#include "TMath.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TTree.h"
#include "TFile.h"
#include "Riostream.h"

void hard_thresh_efficiency(Float_t thresh, char *fname){

	// thresh = hard threshold (cut) in MeV.

	TFile *f0 = TFile::Open(fname);

	TTree *t0 = (TTree *)f0->Get("h1000");
	TTree *t1 = (TTree *)f0->Get("h1001");

	// TH1I *hreact  = new TH1I();
	// TH1F *theta_r = new TH1F();
	// TH1F *g0      = new TH1F();

	t0->Draw("react>>h_react","react == 1","goff");
	Int_t n_react = h_react->GetEntries();

	t0->Draw("recdet>>h_recdet","recdet == 1","goff");
	Int_t n_rec = h_recdet->GetEntries();

	new TCanvas();
	t0->Draw("TMath::ACos(cost_r)>>theta_r","cost_r != 99 && recdet == 1");

	new TCanvas();
	t0->Draw("cost_g>>h","cost_g != 99 && recdet == 1");

	new TCanvas();
	t1->Draw("e0_conv>>g0",Form("e0_conv >= %f && recoil_hit_endv == 1",thresh));
	Int_t n_bgo = g0->GetEntries();

	new TCanvas();
	t1->Draw("e_bgo_second:e_bgo_first>>g1_g0",Form("e_bgo_first >= %f && recoil_hit_endv == 1",thresh),"colz");

	Float_t trans   = n_rec / ((float) n_react);
	Float_t u_trans = trans*sqrt(1/((float) n_rec) + 1/((float)n_react) );
	Float_t eta_bgo = n_bgo / ((float) n_rec);
	Float_t u_eta   = eta_bgo*sqrt(1/((float) n_bgo) + 1/((float) n_rec) );

	cout << "Threshold = " << thresh << "\n";
	cout << "Number of reactions in target = " << n_react << "\n";
	cout << "Number of recoils detected at focal plane = " << n_rec << "\n";
	cout << "Number of coincidences = " << n_bgo << "\n";
	cout << "Separator transmission = " << trans << " +/- " << u_trans << "\n";
	cout << "BGO gamma_0 detection efficiency = " << eta_bgo << " +/- " << u_eta << "\n";


}
