library(growthcurver)

getCountryData = function(data, country) {
    data = data[which(covid_data$location==country, arr.ind = TRUE), ]
    return(data)
}

plotSeasons = function(data, seasons_date) {
    
    opar = par(mfrow=c(1, 1))
    
    df = c(
        "winter" = c(),
        "summer" = "",
        "fall" = "",
        "spring" = ""
    )
    
    index = 1
    for (i in 1:4) {
        seasons = seasons_date[index+2]
        start = which(data$date == seasons_date[index], arr.ind=TRUE)
        end = which(data$date == seasons_date[index+1], arr.ind=TRUE)
        # print(paste(start, ', ', end, ', ', seasons))
        
        if (seasons == 'winter') {
            winter = data[start:end,]
        } else if (seasons == 'summer') {
            summer = data[start:end,]
        } else if (seasons == 'fall') {
            fall = data[start:end,]
        } else {
            spring = data[start:end,]
        }
        
        index = index + 3
    }
    
    plot(winter$new_cases, col="blue", type="l", main="Seasons", ylab="New Cases")
    lines(summer$new_cases, col="red", type="l")
    lines(fall$new_cases, col="orange", type="l")
    lines(spring$new_cases, col="green", type="l")
}

plotVaccinesEffect = function(data, range) {
    opar = par(mfrow=c(1,2))
    index = 1
    
    plot(
        data$new_cases[range[index]:range[index+1]],
        type="l", col="black", 
        main="Before After Vaccines Comparison", ylab="New Cases",
        ylim=c(0, 15)
    )
    lines(dataAmerica$new_cases[range[index+2]:range[index+3]], type="l", col="green")
    
    plot(
        data$new_deaths[range[index]:range[index+1]],
        type="l", col="black", 
        main="Before After Vaccines Comparison", ylab="New Deaths",
        ylim=c(0, 6)
    )
    lines(dataAmerica$new_deaths[range[index+2]:range[index+3]], type="l", col="green")
}

generateModel = function(data, range) {
    index = 1
    df = data.frame(matrix(ncol = 8, nrow = 0))
    
    # seasons
    for (i in 1:4) {
        seasons = range[index+2]
        start= which(data$date == range[index] ,arr.ind=TRUE)
        end = which(data$date == range[index+1],arr.ind=TRUE)
        model_data = data[start:end,]
        
        model_data = model_data[!is.na(model_data$total_cases), ]
        day = 1:dim(model_data)[1]
        model_df = data.frame(day, model_data$total_cases - min(model_data$total_cases))
        gc_fit = SummarizeGrowth(model_df[,1], model_df[,2])
        k = gc_fit$vals$k # k0
        n0 = gc_fit$val$n0 # n0
        r = gc_fit$vals$r # r
        
        log_cdf <- n0*k/(n0+(k-n0)*exp(-r*day))
        tss = sum((model_df[,2] - mean(model_df[,2]))^2)
        rss = sum((model_df[,2] - log_cdf)^2)
        rmse = sum(((log_cdf - model_df[,2])^2)/length(day))
        rsquare = 1-(rss/tss)
        
        index = index + 3
        new_row = c(k, n0, r, tss, rss, rmse, rsquare, seasons)
        df = rbind(df, new_row)
    }
    
    # vaccines effect
    vr = range[13:length(range)]
    index = 1
    
    for(i in 1:2) {
        vd = data[vr[index]:vr[index+1], ]
        vd = vd[!is.na(vd$total_cases), ]
        day = 1:dim(vd)[1]
        
        model_df = data.frame(day, vd$total_cases - min(vd$total_cases))
        
        gc_fit = SummarizeGrowth(model_df[,1], model_df[,2])
        k = gc_fit$vals$k # k0
        n0 = gc_fit$val$n0 # n0
        r = gc_fit$vals$r # r
        
        log_cdf <- n0*k/(n0+(k-n0)*exp(-r*day))
        tss = sum((model_df[,2] - mean(model_df[,2]))^2)
        rss = sum((model_df[,2] - log_cdf)^2)
        rmse = sum(((log_cdf - model_df[,2])^2)/length(day))
        rsquare = 1-(rss/tss)
        
        if (i == 1) {
            category = "Before Vaccines"
        } else {
            category = 'After Vaccines'
        }
        
        new_row = c(k, n0, r, tss, rss, rmse, rsquare, category)
        df = rbind(df, new_row)
        index = index + 2
    }
    
    colnames(df) = c('k', 'n', 'r', 'tss', 'rss', 'rmse', 'rsquare', 'category')
    
    return(df)
}

totalDeathsModel = function(data, range) {
    index = 1
    df = data.frame(matrix(ncol = 8, nrow = 0))
    
    # vaccines effect
    vr = range[1:length(range)]
    index = 1
    
    
    
    for(i in 1:2) {
        vd = data[vr[index]:vr[index+1], ]
        vd = vd[!is.na(vd$total_deaths), ]
        day = 1:dim(vd)[1]
        
        model_df = data.frame(day, vd$total_deaths - min(vd$total_deaths))
        
        gc_fit = SummarizeGrowth(model_df[,1], model_df[,2])
        k = gc_fit$vals$k # k0
        n0 = gc_fit$val$n0 # n0
        r = gc_fit$vals$r # r
        
        log_cdf <- n0*k/(n0+(k-n0)*exp(-r*day))
        tss = sum((model_df[,2] - mean(model_df[,2]))^2)
        rss = sum((model_df[,2] - log_cdf)^2)
        rmse = sum(((log_cdf - model_df[,2])^2)/length(day))
        rsquare = 1-(rss/tss)
        
        if (i == 1) {
            category = "Before Vaccines"
        } else {
            category = 'After Vaccines'
        }
        
        new_row = c(k, n0, r, tss, rss, rmse, rsquare, category)
        df = rbind(df, new_row)
        index = index + 2
    }
    
    colnames(df) = c('k', 'n', 'r', 'tss', 'rss', 'rmse', 'rsquare', 'category')
    
    return(df)
}

describeNewDeaths = function(data, range) {
    index = 1
    
    # vaccines effect
    vr = range[1:length(range)]
    index = 1
    
    for(i in 1:2) {
        vd = data[vr[index]:vr[index+1], ]
        vd = vd[!is.na(vd$total_deaths), ]
        
        if (i == 1) {print('Before Vaccines')}
        else {print('After Vaccines')}
        print(describe(vd$new_deaths))
        index = index + 2
    }
}

checkVaccinesDay = function(data) {
    day = 1:dim(data)[1]
    df = cbind(data, day)
    colnames(df)[length(df)] = 'day'
    
    noNullDf = df[!is.na(df$total_vaccinations), ]
    return(noNullDf$day[1])
}

