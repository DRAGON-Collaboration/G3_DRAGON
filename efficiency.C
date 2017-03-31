#include <string.h>
#include "TFile.h"
#include "TH1.h"
#include "Riostream.h"

TFile *target;
TFile *source;
TFile *result;

// - first define Gaussian convoluting function
float gaus(float energy[], float counts[], int j){
	float factor = sqrt(8.0*log(2.0));
	float k = 0.1733;
	float pi = 3.14159265;
	float sigma = (k*sqrt(energy[j]))/factor;
	float calcGaus[384];
	float sum=0.0;
	for (int i=0; i<384; i++){
		calcGaus[i] = 0;
	}
	for (int i=0; i<384; i++){
		calcGaus[i]=(0.084/(sqrt(2*pi*sigma*sigma)))*(exp(-((energy[i]-energy[j])*(energy[i]-energy[j]))/(2*sigma*sigma)));
	}
	for (int i=0; i<384; i++){
		sum = sum + calcGaus[i]*counts[i];
	}
	return sum;
}


void efficiency(char * pfilename){

	Int_t odbFile = 0;
	cout << "Please Choose the ODB Threshold File Used: " << endl;
	cout << "1) thresh100.odb" << endl;
	cout << "2) thresh150.odb" << endl;
	cout << "3) thresh175.odb" << endl;
	cout << "4) thresh200.odb" << endl;
	cin >> odbFile;

	target = TFile::Open(pfilename);
	result = TFile::Open("results.root","RECREATE");
	target->cd();
	
	
	

	// Create BGO Histograms //
	TH1F *h[32];
	TH1F *hnew[32];
	
	
	
	stringstream ss[32];
	stringstream ss2[32];
	stringstream titl[32];
	stringstream titl2[32];
	for (Int_t i=1; i<32; i++){
		ss[i] << "h" << i;
		titl[i] << "BGO " << i << " spectrum";
		h[i] = new TH1F(ss[i].str().data(),titl[i].str().data(),384,0,15.36);
	}
	for (Int_t i=1; i<32; i++){
		ss2[i] << "hnew" << i;
		titl2[i] << "Convoluted BGO " << i << " spectrum";
		hnew[i] = new TH1F(ss2[i].str().data(),titl2[i].str().data(),384,0,15.36);
	}

	// Plot Individual BGO Spectra into Histograms
	stringstream temp1[32];
	stringstream temp2[32]; 
	for (Int_t i=1; i<32; i++){
		temp1[i] << "e_bgo_first >> h" << i;	
		temp2[i] << "e_bgo_first !=0 & num_bgo_first ==" << i;
		h1001->Draw(temp1[i].str().data(),temp2[i].str().data());
	}

	// Convolute each BGO Histogram for Experimental Resolution	//
	// - then extract energies and counts from histograms
	for (Int_t j=1; j<32; j++){
		float energies[384];
		float counts[384];
		float convolution[384];
		for (Int_t i=0; i<384; i++){
			energies[i] = h[j]->GetBinCenter(i) + 0.0425; //offset
			counts[i] = h[j]->GetBinContent(i);
		}
		for (Int_t i=0; i<384; i++){
			convolution[i] = 0;
			convolution[i] = gaus(energies, counts, i);
			//cout << i << "\t" << j << "\t" << convolution[i] << endl;
		}
		float sum = 0;
		float sumRaw = 0;
		for (Int_t i=0; i<384; i++){
			sum = sum + convolution[i];
			sumRaw = sumRaw + counts[i];
		}
		for (Int_t i=0; i<384; i++){
			if (sum != 0) {
				convolution[i] = convolution[i]*(sumRaw/sum);
			} else {convolution[i] = 0;}
			hnew[j]->SetBinContent(i,convolution[i]);
		}
	}

	// Find out Number of Reactions, and Raw Efficiency 
	// (# gammas that actually generate a non-zero signal)
	Float_t Reactions = h1000->Draw("react","react !=0");
	cout << "Reactions = " << Reactions << endl;
	Float_t RawHits = 0;
	Float_t Max = 0;
	Float_t Max_prev[32];
	Int_t MaxHist = 0;

	for (Int_t j=1; j<32; j++){
		Max_prev[j] = Max;
		RawHits = RawHits + hnew[j]->Integral();
		Max = hnew[j]->GetMaximum();
		if (Max >= Max_prev[MaxHist]) MaxHist = j;
		//cout << Max << "\t" << Max_prev[j] << endl;
	}
 //	cout << MaxHist << endl;

	hnew[MaxHist]->Draw();

	result->cd();

	for (Int_t j=1; j<32; j++){
		if (j != MaxHist) {
			hnew[j]->SetLineColor(j);
			hnew[j]->Draw("Same");
			hnew[j]->Write();
		}
	}

	cout << "RawHits = " << RawHits << endl;
	cout << "Ideal Efficiency (without threshold) = " << 100*RawHits/Reactions << " %" << endl;
	Float_t dReactions = TMath::Sqrt(Reactions);
	// Read in Thresholds
	source = TFile::Open("thresholds.root");
	source->ReadAll();
	
	if (odbFile == 0) {cout << "No ODB File Specified, run again." << endl;}
	else if (odbFile > 4) {cout << "Error, number must be 1-4." << endl;}
	else if (odbFile == 1){
/////////////////////////////////////////////////////////////////////////////////////////////
////// 100 thresh
		func100_0->Draw();
		func100_1->Draw();
		func100_2->Draw();
		func100_3->Draw();
		func100_4->Draw();
		func100_5->Draw();
		func100_6->Draw();
		func100_7->Draw();
		func100_8->Draw();
		func100_9->Draw();
		func100_10->Draw();
		func100_11->Draw();
		func100_12->Draw();
		func100_13->Draw();
		func100_14->Draw();
		func100_15->Draw();
		func100_16->Draw();
		func100_17->Draw();
		func100_18->Draw();
		func100_19->Draw();
		func100_20->Draw();	
		func100_21->Draw();
		func100_22->Draw();
		func100_23->Draw();
		func100_24->Draw();
		func100_25->Draw();
		func100_26->Draw();
		func100_27->Draw();
		func100_28->Draw();
		func100_29->Draw();

		//result->cd();
	
		hnew[6]->Draw();
		hnew[1]->Multiply(func100_0);
		hnew[2]->Multiply(func100_1);
		hnew[3]->Multiply(func100_2);	
		hnew[4]->Multiply(func100_3);
		hnew[5]->Multiply(func100_4);
		hnew[6]->Multiply(func100_5);
		hnew[7]->Multiply(func100_6);
		hnew[8]->Multiply(func100_7);
		hnew[9]->Multiply(func100_8);
		hnew[10]->Multiply(func100_9);
		hnew[11]->Multiply(func100_10);
		hnew[12]->Multiply(func100_11);
		hnew[13]->Multiply(func100_12);
		hnew[14]->Multiply(func100_13);
		hnew[15]->Multiply(func100_14);
		hnew[16]->Multiply(func100_15);
		hnew[17]->Multiply(func100_16);
		hnew[18]->Multiply(func100_17);
		hnew[19]->Multiply(func100_18);
		hnew[20]->Multiply(func100_19);
		hnew[21]->Multiply(func100_20);
		hnew[22]->Multiply(func100_21);
		hnew[23]->Multiply(func100_22);
		hnew[24]->Multiply(func100_23);
		hnew[25]->Multiply(func100_24);
		hnew[26]->Multiply(func100_25);
		hnew[27]->Multiply(func100_26);
		hnew[28]->Multiply(func100_27);
		hnew[29]->Multiply(func100_28);
		hnew[30]->Multiply(func100_29);
	
		Float_t sumnew = 0;
		for (Int_t i=1; i<31; i++){
			sumnew = sumnew + hnew[i]->Integral();
		}
		Float_t dSum = TMath::Sqrt(sumnew)/sumnew;
		cout << "Efficiency = " << sumnew/Reactions << " +/- " << TMath::Sqrt(TMath::Power(dSum,2)+TMath::Power(dReactions/Reactions,2))*sumnew/Reactions << endl;
	}
	else if (odbFile == 2){
/////////////////////////////////////////////////////////////////////////////////////////////
////// 150 thresh
		func150_0->Draw();
		func150_1->Draw();
		func150_2->Draw();
		func150_3->Draw();
		func150_4->Draw();
		func150_5->Draw();
		func150_6->Draw();
		func150_7->Draw();
		func150_8->Draw();
		func150_9->Draw();
		func150_10->Draw();
		func150_11->Draw();
		func150_12->Draw();
		func150_13->Draw();
		func150_14->Draw();
		func150_15->Draw();
		func150_16->Draw();
		func150_17->Draw();
		func150_18->Draw();
		func150_19->Draw();
		func150_20->Draw();	
		func150_21->Draw();
		func150_22->Draw();
		func150_23->Draw();
		func150_24->Draw();
		func150_25->Draw();
		func150_26->Draw();
		func150_27->Draw();
		func150_28->Draw();
		func150_29->Draw();

		//result->cd();
	
		hnew[6]->Draw();
		hnew[1]->Multiply(func150_0);
		hnew[2]->Multiply(func150_1);
		hnew[3]->Multiply(func150_2);	
		hnew[4]->Multiply(func150_3);
		hnew[5]->Multiply(func150_4);
		hnew[6]->Multiply(func150_5);
		hnew[7]->Multiply(func150_6);
		hnew[8]->Multiply(func150_7);
		hnew[9]->Multiply(func150_8);
		hnew[10]->Multiply(func150_9);
		hnew[11]->Multiply(func150_10);
		hnew[12]->Multiply(func150_11);
		hnew[13]->Multiply(func150_12);
		hnew[14]->Multiply(func150_13);
		hnew[15]->Multiply(func150_14);
		hnew[16]->Multiply(func150_15);
		hnew[17]->Multiply(func150_16);
		hnew[18]->Multiply(func150_17);
		hnew[19]->Multiply(func150_18);
		hnew[20]->Multiply(func150_19);
		hnew[21]->Multiply(func150_20);
		hnew[22]->Multiply(func150_21);
		hnew[23]->Multiply(func150_22);
		hnew[24]->Multiply(func150_23);
		hnew[25]->Multiply(func150_24);
		hnew[26]->Multiply(func150_25);
		hnew[27]->Multiply(func150_26);
		hnew[28]->Multiply(func150_27);
		hnew[29]->Multiply(func150_28);
		hnew[30]->Multiply(func150_29);
	
		Float_t sumnew = 0;
		for (Int_t i=1; i<31; i++){
			sumnew = sumnew + hnew[i]->Integral();
		}
		Float_t dSum = TMath::Sqrt(sumnew)/sumnew;
                cout << "Efficiency = " << sumnew/Reactions << " +/- " << TMath::Sqrt(TMath::Power(dSum,2)+TMath::Power(dReactions/Reactions,2))*sumnew/Reactions << endl;
	}
	else if (odbFile == 3){
/////////////////////////////////////////////////////////////////////////////////////////////
////// 175 thresh
	
		func175_0->Draw();
		func175_1->Draw();
		func175_2->Draw();
		func175_3->Draw();
		func175_4->Draw();
		func175_5->Draw();
		func175_6->Draw();
		func175_7->Draw();
		func175_8->Draw();
		func175_9->Draw();
		func175_10->Draw();
		func175_11->Draw();
		func175_12->Draw();
		func175_13->Draw();
		func175_14->Draw();
		func175_15->Draw();
		func175_16->Draw();
		func175_17->Draw();
		func175_18->Draw();
		func175_19->Draw();
		func175_20->Draw();	
		func175_21->Draw();
		func175_22->Draw();
		func175_23->Draw();
		func175_24->Draw();
		func175_25->Draw();
		func175_26->Draw();
		func175_27->Draw();
		func175_28->Draw();
		func175_29->Draw();

		//result->cd();
	
		hnew[6]->Draw();
		hnew[1]->Multiply(func175_0);
		hnew[2]->Multiply(func175_1);
		hnew[3]->Multiply(func175_2);	
		hnew[4]->Multiply(func175_3);
		hnew[5]->Multiply(func175_4);
		hnew[6]->Multiply(func175_5);
		hnew[7]->Multiply(func175_6);
		hnew[8]->Multiply(func175_7);
		hnew[9]->Multiply(func175_8);
		hnew[10]->Multiply(func175_9);
		hnew[11]->Multiply(func175_10);
		hnew[12]->Multiply(func175_11);
		hnew[13]->Multiply(func175_12);
		hnew[14]->Multiply(func175_13);
		hnew[15]->Multiply(func175_14);
		hnew[16]->Multiply(func175_15);
		hnew[17]->Multiply(func175_16);
		hnew[18]->Multiply(func175_17);
		hnew[19]->Multiply(func175_18);
		hnew[20]->Multiply(func175_19);
		hnew[21]->Multiply(func175_20);
		hnew[22]->Multiply(func175_21);
		hnew[23]->Multiply(func175_22);
		hnew[24]->Multiply(func175_23);
		hnew[25]->Multiply(func175_24);
		hnew[26]->Multiply(func175_25);
		hnew[27]->Multiply(func175_26);
		hnew[28]->Multiply(func175_27);
		hnew[29]->Multiply(func175_28);
		hnew[30]->Multiply(func175_29);
	
		Float_t sumnew = 0;
		for (Int_t i=1; i<31; i++){
			sumnew = sumnew + hnew[i]->Integral();
		}
                Float_t dSum = TMath::Sqrt(sumnew)/sumnew;
		cout << "Efficiency = " << sumnew/Reactions << " +/- " << TMath::Sqrt(TMath::Power(dSum,2)+TMath::Power(dReactions/Reactions,2))*sumnew/Reactions << endl;
	}
	else if (odbFile == 4){
/////////////////////////////////////////////////////////////////////////////////////////////
////// 200 thresh
		func200_0->Draw();
		func200_1->Draw();
		func200_2->Draw();
		func200_3->Draw();
		func200_4->Draw();
		func200_5->Draw();
		func200_6->Draw();
		func200_7->Draw();
		func200_8->Draw();
		func200_9->Draw();
		func200_10->Draw();
		func200_11->Draw();
		func200_12->Draw();
		func200_13->Draw();
		func200_14->Draw();
		func200_15->Draw();
		func200_16->Draw();
		func200_17->Draw();
		func200_18->Draw();
		func200_19->Draw();
		func200_20->Draw();	
		func200_21->Draw();
		func200_22->Draw();
		func200_23->Draw();
		func200_24->Draw();
		func200_25->Draw();
		func200_26->Draw();
		func200_27->Draw();
		func200_28->Draw();
		func200_29->Draw();

		//result->cd();
	
		hnew[6]->Draw();
		hnew[1]->Multiply(func200_0);
		hnew[2]->Multiply(func200_1);
		hnew[3]->Multiply(func200_2);	
		hnew[4]->Multiply(func200_3);
		hnew[5]->Multiply(func200_4);
		hnew[6]->Multiply(func200_5);
		hnew[7]->Multiply(func200_6);
		hnew[8]->Multiply(func200_7);
		hnew[9]->Multiply(func200_8);
		hnew[10]->Multiply(func200_9);
		hnew[11]->Multiply(func200_10);
		hnew[12]->Multiply(func200_11);
		hnew[13]->Multiply(func200_12);
		hnew[14]->Multiply(func200_13);
		hnew[15]->Multiply(func200_14);
		hnew[16]->Multiply(func200_15);
		hnew[17]->Multiply(func200_16);
		hnew[18]->Multiply(func200_17);
		hnew[19]->Multiply(func200_18);
		hnew[20]->Multiply(func200_19);
		hnew[21]->Multiply(func200_20);
		hnew[22]->Multiply(func200_21);
		hnew[23]->Multiply(func200_22);
		hnew[24]->Multiply(func200_23);
		hnew[25]->Multiply(func200_24);
		hnew[26]->Multiply(func200_25);
		hnew[27]->Multiply(func200_26);
		hnew[28]->Multiply(func200_27);
		hnew[29]->Multiply(func200_28);
		hnew[30]->Multiply(func200_29);
	
		Float_t sumnew = 0;
		for (Int_t i=1; i<31; i++){
			sumnew = sumnew + hnew[i]->Integral();
		}
                Float_t dSum = TMath::Sqrt(sumnew)/sumnew;
		cout << "Efficiency = " << sumnew/Reactions << " +/- " << TMath::Sqrt(TMath::Power(dSum,2)+TMath::Power(dReactions/Reactions,2))*sumnew/Reactions << endl;
	}
	
}
