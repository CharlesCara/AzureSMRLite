
context("Testing azureSMR Blob commands")

test_that("No category", {

  blobObject <- list(a = "1", b = "2")
  blobName <- "testBlob"
  resourceGroup = "asrQuantProduction"
  containerName = "testdata"
  storageAccount = "asrquantstorage"
  verbose <- FALSE

  sc <- createAzureContext(tenantID = Sys.getenv("azureTenantID"),
                           clientID = Sys.getenv("azureClientID"),
                           authKey= Sys.getenv("azureAuthKey"),
                           authType = "ClientCredential")
  expect_false(is.null(sc))

  sk <- azureSAGetKey(azureActiveContext = sc,
                      resourceGroup = resourceGroup,
                      storageAccount = storageAccount,
                      verbose = verbose)

  expect_false(is.null(sk))

  con <- list(sc = sc,
              sk = sk)

  assign(x = ".azureConnection",
         value = con,
         envir = .GlobalEnv)

  ret <- azurePutBlob(azureActiveContext = con$sc,
               storageKey = con$sk,
               storageAccount = storageAccount,
               container = containerName,
               contents = jsonlite::toJSON(blobObject),
               blob = blobName,
               verbose = verbose)
  expect_true(ret)

  listBlobs <- azureListStorageBlobs(azureActiveContext = con$sc,
                                     storageKey = con$sk,
                                     storageAccount = storageAccount,
                                     container = containerName,
                                     prefix = blobName,
                                     verbose = verbose)
  expect_is(listBlobs, "data.frame")

  expBlobObject <- jsonlite::fromJSON(azureGetBlob(azureActiveContext = con$sc,
                                       storageKey = con$sk,
                                       storageAccount = storageAccount,
                                       container = containerName,
                                       blob = blobName,
                                       type = "text",
                                       verbose = verbose) )

  expect_equal(blobObject, expBlobObject)

  ret <- azureDeleteBlob(azureActiveContext = con$sc,
                  storageAccount = storageAccount,
                  container = containerName,
                  blob = blobName,
                  verbose = verbose)
  expect_true(ret)

  if(exists(x = ".azureConnection", envir = .GlobalEnv)){
    rm(".azureConnection", envir = .GlobalEnv)
  }
})


